#' Synthesize imaginary participants
#'
#' Runs the checker, then builds a whole fake study: screens people until
#' `n_subjects` are enrolled, simulates their latent trajectories and item
#' responses, applies dropout, missed visits, abandoned forms, skipped items
#' and branching logic, and fills open-text fields. Nothing here ever looks
#' at real data.
#'
#' The order of operations: complete data first, then abandoned forms and
#' skipped items (so a half-finished screening form can make someone
#' ineligible), calculated fields, screening, withdrawal, missed visits, and
#' finally branching logic.
#'
#' @param spec A spec, or a path to one.
#' @param n Number of participants to enroll. Defaults to
#'   `design$n_subjects`.
#' @param seed Random seed. Defaults to the spec's `seed`. Your session's
#'   random number state is restored afterward.
#' @param check Run [check_spec()] first and refuse to continue on errors?
#' @param quiet Suppress the friendly progress messages?
#' @return A tibble in REDCap import format, with class `studysynth_data`.
#'   [study_flow()] tells you who made it through; [true_scores()] gives the
#'   latent values behind the responses.
#' @export
#' @examples
#' fake <- synthesize(example_path("calm_minds.yaml"), n = 10)
#' study_flow(fake)
synthesize <- function(spec, n = NULL, seed = NULL, check = TRUE, quiet = FALSE) {
  spec <- as_spec(spec)
  project <- spec_project(spec)
  if (!is.null(n)) spec$design$n_subjects <- n
  n <- spec$design$n_subjects %||% 100
  seed <- seed %||% spec$seed
  if (!is.null(seed)) withr::local_seed(seed)

  if (check) {
    # The checker draws a few random numbers; keep them from changing the data.
    issues <- withr::with_preserve_seed(check_spec(spec, project, smoke = FALSE))
    if (any(issues$level == "error")) {
      print(issues)
      cli::cli_abort("The spec has errors, so I didn't synthesize anything. See the issues above.")
    }
  }

  if (!quiet) cli::cli_alert_info("Inventing {n} participant{?s} for {.strong {spec$title %||% 'your study'}}. None of them are real.")

  # Screen people in batches until enough are enrolled.
  next_id <- spec$design$id_start %||% 1
  batches <- list()
  enrolled <- 0
  for (attempt in 1:50) {
    ids <- seq(next_id, length.out = max(n - enrolled, 1) * 2)
    batch <- simulate_batch(spec, project, ids)
    batches <- c(batches, list(batch))
    enrolled <- enrolled + sum(batch$status$status %in% enrolled_statuses)
    next_id <- max(ids) + 1
    if (enrolled >= n) break
  }
  if (enrolled < n) {
    cli::cli_abort("Only {enrolled} of {n} people passed screening after 50 batches. Is {.field eligible_if} too strict?")
  }

  status <- purrr::map_dfr(batches, "status")
  # Keep everyone screened up to and including the nth enrollee.
  last_id <- status$record_id[which(cumsum(status$status %in% enrolled_statuses) == n)[1]]
  keep <- status$record_id[seq_len(which(status$record_id == last_id))]
  status <- dplyr::filter(status, record_id %in% keep)
  records <- purrr::map_dfr(batches, "records") |> dplyr::filter(record_id %in% keep)
  occasions <- purrr::map_dfr(batches, "occasions") |> dplyr::filter(record_id %in% keep)

  out <- format_output(project, records)
  attr(out, "status") <- status
  attr(out, "occasions") <- occasions
  attr(out, "spec") <- spec
  class(out) <- c("studysynth_data", class(out))
  if (!quiet) cli::cli_alert_success("Done: {nrow(out)} rows. Try {.fn study_flow} or {.fn plot_trajectories}.")
  out
}

simulate_batch <- function(spec, project, ids) {
  people <- make_people(spec, ids)
  occasions <- make_occasions(spec, project, people) |>
    add_latents(spec = spec) |>
    apply_compliance(spec = spec)
  records <- make_records(spec, project, occasions, people) |>
    apply_abandonment(spec = spec, project = project) |>
    apply_skipped_items(spec = spec, project = project) |>
    compute_calcs(project = project)

  screened <- apply_screening(spec, project, records, people)
  records <- screened$records
  status <- screened$status

  withdrawn <- apply_withdrawal(spec, project, records, occasions, status)
  records <- withdrawn$records
  status <- withdrawn$status

  records <- records |>
    apply_missed_visits(spec = spec, project = project) |>
    compute_calcs(project = project) |>
    apply_branching(project = project) |>
    add_open_text(spec = spec, project = project, occasions = occasions)

  list(records = records, status = status, occasions = occasions)
}

# Screening ---------------------------------------------------------------

# Everyone starts "completed"; screening and withdrawal change that.
enrolled_statuses <- c("completed", "withdrew")

apply_screening <- function(spec, project, records, people) {
  s <- spec$screening
  status <- tibble::tibble(record_id = people$record_id, status = "completed",
                           withdrawn_at = NA_character_)
  if (is.null(s)) {
    return(list(records = records, status = status))
  }

  screen_rows <- records$redcap_event_name == s$event & is.na(records$redcap_repeat_instance)
  screen <- records[screen_rows, ]
  eligible <- if (is.null(s$eligible_if)) {
    rep(TRUE, nrow(screen))
  } else {
    eval_redcap_logic(s$eligible_if, screen) %in% TRUE
  }
  declined <- eligible & stats::runif(nrow(screen)) < (s$decline_prob %||% 0)
  if (!is.null(s$consent_field)) {
    # Walked away before answering the consent question: not enrolled.
    declined <- declined | (eligible & is.na(screen[[s$consent_field]]))
  }

  status$status[match(screen$record_id[!eligible], status$record_id)] <- "ineligible"
  status$status[match(screen$record_id[declined], status$record_id)] <- "declined"

  # Record consent: yes for enrollees, no for decliners, blank (the consent
  # form never opened) for ineligible people.
  if (!is.null(s$consent_field)) {
    info <- field_info(project, s$consent_field)
    consent_cols <- form_columns(project, info$form_name, names(records))
    rows <- which(screen_rows)
    answered <- !is.na(screen[[s$consent_field]])
    records[[s$consent_field]][rows[answered]] <- dplyr::if_else(declined[answered], "0", "1")
    records[rows[!eligible], c(consent_cols, complete_field(info$form_name))] <- NA
  }

  not_enrolled <- status$record_id[!status$status %in% enrolled_statuses]
  records <- records[!(records$record_id %in% not_enrolled & !screen_rows), ]
  list(records = records, status = status)
}

# Missingness -------------------------------------------------------------

# Events after enrollment (later than the screening event), in time order.
followup_events <- function(spec, project) {
  events <- study_events(spec, project)
  screen_event <- spec$screening$event
  if (!is.null(screen_event)) {
    events <- dplyr::filter(events, day_offset > events$day_offset[events$unique_event_name == screen_event])
  }
  group_event <- spec$design$groups$event
  if (!is.null(group_event)) {
    events <- dplyr::filter(events, day_offset > events$day_offset[events$unique_event_name == group_event])
  }
  events$unique_event_name
}

apply_withdrawal <- function(spec, project, records, occasions, status) {
  w <- spec$missingness$withdrawal
  if (is.null(w)) {
    return(list(records = records, status = status))
  }
  events <- followup_events(spec, project)
  enrolled <- status$record_id[status$status %in% enrolled_statuses]
  # Latent state at each event (averaged over prompts for sampling bursts).
  state <- occasions |>
    dplyr::filter(record_id %in% enrolled, unique_event_name %in% events) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(names(w$depends_on)), mean),
                     .by = c(record_id, unique_event_name))

  for (event in events) {
    at_risk <- dplyr::filter(state, unique_event_name == event,
                             record_id %in% status$record_id[status$status == "completed"])
    logit <- stats::qlogis(w$per_event) + depends_on_effect(w$depends_on, at_risk)
    leaving <- at_risk$record_id[stats::runif(nrow(at_risk)) < stats::plogis(logit)]
    status$status[status$record_id %in% leaving] <- "withdrew"
    status$withdrawn_at[status$record_id %in% leaving] <- event
  }

  # Drop each withdrawn person's rows from their withdrawal event onward.
  event_order <- match(records$redcap_event_name, study_events(spec, project)$unique_event_name)
  cutoff <- match(status$withdrawn_at, study_events(spec, project)$unique_event_name)
  cutoff <- cutoff[match(records$record_id, status$record_id)]
  records <- records[is.na(cutoff) | event_order < cutoff, ]
  list(records = records, status = status)
}

apply_missed_visits <- function(records, spec, project) {
  p <- spec$missingness$missed_visit %||% 0
  if (p == 0) return(records)
  ema_events <- purrr::map_chr(spec$ema, "event")
  candidates <- records |>
    dplyr::filter(redcap_event_name %in% setdiff(followup_events(spec, project), ema_events)) |>
    dplyr::distinct(record_id, redcap_event_name)
  missed <- candidates[stats::runif(nrow(candidates)) < p, ]
  dplyr::anti_join(records, missed, by = c("record_id", "redcap_event_name"))
}

# Someone stops partway through a form: everything from a random item onward
# is blank and the form is marked Incomplete (0). Later items are likelier
# break points.
apply_abandonment <- function(records, spec, project) {
  a <- spec$missingness$abandon
  if (is.null(a)) return(records)
  for (form in unique(project$fields$form_name)) {
    status_col <- complete_field(form)
    if (!status_col %in% names(records)) next
    p <- a$by_form[[form]] %||% a$prob %||% 0
    columns <- form_columns(project, form, names(records))
    columns <- setdiff(columns, calc_fields(project))
    rows <- which(!is.na(records[[status_col]]) & stats::runif(nrow(records)) < p)
    if (length(rows) == 0) next
    starts <- sample(seq_along(columns), length(rows), replace = TRUE, prob = seq_along(columns))
    # Column by column: blank it in every abandoned row that broke off at or
    # before it.
    for (i in seq_along(columns)) {
      records[[columns[i]]][rows[starts <= i]] <- NA
    }
    records[[status_col]][rows] <- "0"
  }
  records
}

apply_skipped_items <- function(records, spec, project) {
  s <- spec$missingness$skip_item
  if (is.null(s)) return(records)
  items <- unlist(purrr::map(spec$measurement, \(block) parse_measurement(block)$items))
  for (item in intersect(items, names(records))) {
    p <- s$by_item[[item]] %||% s$prob %||% 0
    records[[item]][stats::runif(nrow(records)) < p] <- NA
  }
  records
}

calc_fields <- function(project) {
  project$fields$field_name[project$fields$field_type == "calc"]
}

# Output ------------------------------------------------------------------

# Columns in REDCap import order: keys, then each form's fields followed by
# its _complete status.
format_output <- function(project, records) {
  columns <- unlist(purrr::map(unique(project$fields$form_name), \(form) {
    c(form_columns(project, form, names(records)), complete_field(form))
  }))
  columns <- intersect(columns, names(records))
  id_field <- project$fields$field_name[1]
  out <- records |>
    dplyr::select(dplyr::all_of(row_keys), dplyr::all_of(columns)) |>
    dplyr::rename(!!id_field := record_id)
  if (all(is.na(out$redcap_repeat_instance))) {
    out <- dplyr::select(out, -redcap_repeat_instrument, -redcap_repeat_instance)
  }
  if (nrow(project$events) == 1) out <- dplyr::select(out, -redcap_event_name)
  tibble::as_tibble(out)
}

#' Write synthetic data as a REDCap import CSV
#'
#' Also writes a small manifest next to it (`<file>.manifest.yaml`) recording
#' the spec, seed, and package version, so every synthetic file traces back to
#' the spec that made it.
#'
#' @param data Output of [synthesize()].
#' @param path Where to write the CSV.
#' @return `path`, invisibly.
#' @export
#' @examples
#' fake <- synthesize(example_path("calm_minds.yaml"), n = 10, quiet = TRUE)
#' write_redcap_csv(fake, tempfile(fileext = ".csv"))
write_redcap_csv <- function(data, path) {
  readr::write_csv(tibble::as_tibble(unclass_data(data)), path, na = "")
  spec <- attr(data, "spec")
  manifest <- list(
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    studysynth_version = as.character(utils::packageVersion("studysynth")),
    spec_hash = digest::digest(unclass(spec)),
    seed = spec$seed,
    n_rows = nrow(data),
    flow = as.list(table(attr(data, "status")$status)),
    note = "Synthetic data. No real participants were involved."
  )
  yaml::write_yaml(manifest, paste0(path, ".manifest.yaml"))
  invisible(path)
}

unclass_data <- function(data) {
  attributes(data)[c("status", "occasions", "spec")] <- NULL
  class(data) <- setdiff(class(data), "studysynth_data")
  data
}
