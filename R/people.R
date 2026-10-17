# Imaginary people and the occasions we "see" them: visits and prompts.

make_people <- function(spec, ids) {
  n <- length(ids)
  start <- as.Date(spec$design$start_date %||% "2026-01-05")
  enroll_window <- spec$design$enroll_days %||% 180
  people <- tibble::tibble(
    record_id = as.character(ids),
    day0 = start + sample(0:enroll_window, n, replace = TRUE)
  )

  for (name in names(spec$covariates)) {
    people[[name]] <- draw_covariate(spec$covariates[[name]], n)
  }

  groups <- spec$design$groups
  if (!is.null(groups)) {
    probs <- unlist(groups$probs)
    people[[groups$field]] <- factor(
      sample(names(probs), n, replace = TRUE, prob = probs),
      levels = names(probs)
    )
  }
  people
}

draw_covariate <- function(def, n) {
  value <- switch(def$dist,
    normal = stats::rnorm(n, def$mean %||% 0, def$sd %||% 1),
    uniform = stats::runif(n, def$min %||% 0, def$max %||% 1),
    binary = as.numeric(stats::runif(n) < (def$prob %||% 0.5)),
    categorical = {
      probs <- unlist(def$probs)
      return(factor(sample(names(probs), n, replace = TRUE, prob = probs), levels = names(probs)))
    },
    cli::cli_abort("Unknown covariate distribution {.val {def$dist}}.")
  )
  if (!is.null(def$min)) value <- pmax(value, def$min)
  if (!is.null(def$max)) value <- pmin(value, def$max)
  if (!is.null(def$round)) value <- round(value, def$round)
  value
}

# One row per person per visit (non-repeating forms) and per experience
# sampling prompt. Prompts are all scheduled here; which ones get answered is
# decided later by `apply_compliance()`, since that can depend on mood.
make_occasions <- function(spec, project, people) {
  visit_events <- study_events(spec, project) |>
    dplyr::filter(purrr::map_lgl(unique_event_name, \(event) {
      length(visit_forms(spec, project, event)) > 0
    }))

  visits <- tidyr::expand_grid(people, visit_events) |>
    dplyr::mutate(
      ema = 0,
      ema_block = NA_character_,
      prompt_day = NA_integer_,
      hour = stats::runif(dplyr::n(), 9, 17)
    )

  prompts <- purrr::imap_dfr(spec$ema, \(block, name) {
    schedule_prompts(block, name, people, project)
  })

  unit <- spec$design$time$unit_days %||% 1
  dplyr::bind_rows(visits, prompts) |>
    dplyr::mutate(
      day = day_offset + dplyr::coalesce(prompt_day - 1, 0),
      datetime = as.POSIXct(day0 + day, tz = "UTC") + hour * 3600,
      time = day / unit,
      sin_tod = sin(2 * pi * hour / 24),
      cos_tod = cos(2 * pi * hour / 24),
      weekend = as.numeric(format(day0 + day, "%u") %in% c("6", "7"))
    ) |>
    dplyr::arrange(record_id, datetime) |>
    dplyr::mutate(occasion_id = dplyr::row_number())
}

# Events for the arm we simulate (one arm per spec, for now), in time order.
study_events <- function(spec, project) {
  project$events |>
    dplyr::filter(arm_num == (spec$design$arm %||% 1)) |>
    dplyr::arrange(day_offset)
}

# Forms filled at an ordinary visit to an event: everything except forms
# that experience sampling blocks fill prompt by prompt.
visit_forms <- function(spec, project, event) {
  setdiff(forms_at_event(project, event), unlist(purrr::map(spec$ema, \(block) {
    if (block$event == event) prompt_forms(block, project)
  })))
}

# Forms filled at each prompt: the block's `form` (a repeating instrument),
# or every form at the event (a repeating event).
prompt_forms <- function(block, project) {
  block$form %||% forms_at_event(project, block$event)
}

schedule_prompts <- function(block, name, people, project) {
  event <- project$events[project$events$unique_event_name == block$event, ]
  per_day <- block$prompts_per_day %||% 5
  window <- block$window %||% c("09:00", "21:00")
  window_hours <- purrr::map_dbl(window, \(t) {
    parts <- as.numeric(strsplit(t, ":")[[1]])
    parts[1] + parts[2] / 60
  })
  slice <- diff(window_hours) / per_day

  people |>
    dplyr::mutate(n_days = sample_days(block$days, dplyr::n())) |>
    dplyr::reframe(
      prompt_day = rep(seq_len(n_days), each = per_day),
      slot = rep(seq_len(per_day), times = n_days),
      .by = dplyr::everything()
    ) |>
    dplyr::mutate(
      # Signal-contingent: one random time inside each equal slice of the day.
      hour = window_hours[1] + (slot - 1 + stats::runif(dplyr::n())) * slice,
      ema = 1,
      ema_block = name,
      unique_event_name = event$unique_event_name,
      event_name = event$event_name,
      arm_num = event$arm_num,
      day_offset = event$day_offset
    ) |>
    dplyr::select(-n_days, -slot)
}

sample_days <- function(days, n) {
  if (is.numeric(days)) return(rep(days, n))
  lo <- days$min %||% 7
  hi <- days$max %||% lo
  if (lo == hi) rep(lo, n) else sample(lo:hi, n, replace = TRUE)
}

# Decide which prompts got answered. A typical person answers `typical` of
# prompts; people differ (`person_sd`, logit scale), answering drops off over
# days (`per_day`), and can depend on latent state (`depends_on`).
apply_compliance <- function(spec, occasions) {
  answered <- rep(TRUE, nrow(occasions))
  for (name in names(spec$ema)) {
    c <- spec$ema[[name]]$compliance %||% list()
    rows <- which(occasions$ema_block %in% name)
    if (length(rows) == 0) next
    people <- unique(occasions$record_id)
    person_effect <- stats::rnorm(length(people), 0, c$person_sd %||% 0)
    logit <- stats::qlogis(c$typical %||% 0.8) +
      person_effect[match(occasions$record_id[rows], people)] +
      (c$per_day %||% 0) * (occasions$prompt_day[rows] - 1) +
      depends_on_effect(c$depends_on, occasions)[rows]
    answered[rows] <- stats::runif(length(rows)) < stats::plogis(logit)
  }
  occasions[answered, ]
}

# Sum of coefficient * latent value, for missingness that depends on state.
depends_on_effect <- function(depends_on, data) {
  effect <- rep(0, nrow(data))
  for (name in names(depends_on)) {
    effect <- effect + depends_on[[name]] * data[[name]]
  }
  effect
}
