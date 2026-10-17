#' Check a spec against its REDCap project
#'
#' One deterministic checker for everyone who touches a spec: you, your
#' advisor, the LLM drafting it, the Shiny editor, a git hook. It checks, in
#' order:
#'
#' 1. Shape: known blocks, numbers where numbers belong.
#' 2. References: every field, form, and event exists in the REDCap project,
#'    items have the right types, threshold counts match choices.
#' 3. Validity: lavaan models parse with fixed loadings, error covariances
#'    are positive definite, formula coefficient names match the design
#'    matrix, random-effect covariances are valid.
#' 4. A smoke test: synthesize 20 people and make sure nothing breaks.
#' 5. Optionally, plausibility: simulate a bigger sample and compare with
#'    `expect:` values pulled from your sources.
#'
#' @param spec A spec, or a path to one.
#' @param project The `redcap_project`. Read from the spec if omitted.
#' @param smoke Run the smoke test?
#' @param plausibility Check `expect:` values against a simulated sample?
#' @return A tibble of issues with columns `level` (error, warning, info),
#'   `path` (where in the spec), `message`, and `hint`.
#' @export
#' @examples
#' check_spec(example_path("calm_minds.yaml"))
check_spec <- function(spec, project = NULL, smoke = TRUE, plausibility = FALSE) {
  spec <- as_spec(spec)
  if (is.null(project)) {
    project <- tryCatch(spec_project(spec), error = \(e) e)
    if (inherits(project, "error")) {
      return(as_issues(issue("error", "redcap", conditionMessage(project),
                             "Check the paths in the redcap: block; they're relative to the spec file.")))
    }
  }

  issues <- dplyr::bind_rows(
    check_shape(spec),
    check_design(spec, project),
    check_covariates(spec, project),
    check_measurement(spec, project),
    check_latents(spec, project),
    check_ema(spec, project),
    check_screening(spec, project),
    check_missingness(spec, project),
    check_open_text(spec, project),
    check_sources(spec),
    check_coverage(spec, project)
  )

  ok_so_far <- !any(issues$level == "error")
  if (smoke && ok_so_far) {
    issues <- dplyr::bind_rows(issues, check_smoke(spec))
  }
  if (plausibility && !any(issues$level == "error")) {
    issues <- dplyr::bind_rows(issues, check_expectations(spec, project))
  }
  as_issues(issues)
}

issue <- function(level, path, message, hint = NA_character_) {
  tibble::tibble(level = level, path = path, message = message, hint = hint)
}

no_issues <- function() issue(character(), character(), character(), character())

as_issues <- function(x) {
  x <- dplyr::bind_rows(no_issues(), x)
  x <- dplyr::arrange(x, factor(level, c("error", "warning", "info")))
  class(x) <- c("studysynth_issues", class(x))
  x
}

#' @export
print.studysynth_issues <- function(x, ...) {
  n_err <- sum(x$level == "error")
  n_warn <- sum(x$level == "warning")
  for (i in seq_len(nrow(x))) {
    symbol <- switch(x$level[i], error = cli::col_red("x"), warning = cli::col_yellow("!"), cli::col_blue("i"))
    cli::cat_line(symbol, " ", cli::style_bold(x$path[i]), ": ", x$message[i])
    if (!is.na(x$hint[i])) cli::cat_line("    ", cli::col_grey(x$hint[i]))
  }
  if (n_err == 0 && n_warn == 0) {
    cli::cli_alert_success("Spec looks good. Go make some imaginary people.")
  } else if (n_err == 0) {
    cli::cli_alert_success("No errors ({n_warn} warning{?s}). The spec will run.")
  } else {
    cli::cli_alert_danger("{n_err} error{?s}, {n_warn} warning{?s}. Fix the errors before synthesizing.")
  }
  invisible(x)
}

#' Check a spec from the command line
#'
#' For git hooks, CI, and session-start hooks: prints the issues and exits
#' with status 1 if there are errors.
#'
#' @param path Path to the spec.
#' @param smoke Run the smoke test? Off by default to keep hooks fast.
#' @param exit_code Exit status when there are errors. Claude Code hooks use
#'   2 to send the errors back to the model; 0 just reports.
#' @export
check_spec_cli <- function(path = "spec.yaml", smoke = FALSE, exit_code = 1) {
  issues <- check_spec(path, smoke = smoke)
  print(issues)
  if (any(issues$level == "error") && !interactive()) quit(status = exit_code)
  invisible(issues)
}

# Helpers -----------------------------------------------------------------

known_blocks <- c("studysynth", "title", "seed", "redcap", "design", "covariates",
                  "measurement", "latents", "ema", "screening", "missingness",
                  "open_text", "output", "notes")

is_prob <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 && x <= 1

check_prob <- function(x, path) {
  if (is.null(x) || is_prob(x)) return(NULL)
  issue("error", path, "must be a probability between 0 and 1.")
}

has_field <- function(project, field) field %in% project$fields$field_name
has_form <- function(project, form) form %in% project$fields$form_name
has_event <- function(project, event) event %in% project$events$unique_event_name

missing_field <- function(project, field, path) {
  if (has_field(project, field)) return(NULL)
  close <- agrep(field, project$fields$field_name, max.distance = 0.2, value = TRUE)
  issue("error", path, sprintf("field '%s' isn't in the data dictionary.", field),
        if (length(close) > 0) sprintf("Did you mean '%s'?", close[1]) else NA_character_)
}

missing_event <- function(project, event, path) {
  if (!is.null(event) && has_event(project, event)) return(NULL)
  issue("error", path, sprintf("event '%s' isn't in the project.", event %||% ""),
        paste("Events are:", paste(project$events$unique_event_name, collapse = ", ")))
}

latent_names <- function(spec) names(spec$latents)

# Shape -------------------------------------------------------------------

check_shape <- function(spec) {
  unknown <- setdiff(names(spec), known_blocks)
  dplyr::bind_rows(
    purrr::map(unknown, \(name) issue("warning", name, "isn't a block studysynth knows; it will be ignored.",
                                       paste("Known blocks:", paste(known_blocks, collapse = ", ")))),
    if (is.null(spec$studysynth)) issue("info", "studysynth", "no spec version given; assuming 0.1."),
    if (!is.null(spec$seed) && !is.numeric(spec$seed)) issue("error", "seed", "must be a number.")
  )
}

# Design ------------------------------------------------------------------

check_design <- function(spec, project) {
  d <- spec$design
  out <- list()
  n <- d$n_subjects
  if (!is.null(n) && !(is.numeric(n) && n >= 1 && n == round(n))) {
    out <- c(out, list(issue("error", "design.n_subjects", "must be a whole number, at least 1.")))
  }
  if (!is.null(d$start_date) && is.na(suppressWarnings(as.Date(as.character(d$start_date), optional = TRUE)))) {
    out <- c(out, list(issue("error", "design.start_date", "isn't a date like 2026-01-05.")))
  }
  if (!is.null(d$time$unit_days) && !(is.numeric(d$time$unit_days) && d$time$unit_days > 0)) {
    out <- c(out, list(issue("error", "design.time.unit_days", "must be a positive number.")))
  }

  g <- d$groups
  if (!is.null(g)) {
    out <- c(out, list(missing_field(project, g$field, "design.groups.field")))
    probs <- unlist(g$probs)
    if (is.null(probs)) {
      out <- c(out, list(issue("error", "design.groups.probs", "list each group's code and probability, like {\"1\": 0.5, \"2\": 0.5}.")))
    } else if (abs(sum(probs) - 1) > 0.001) {
      out <- c(out, list(issue("warning", "design.groups.probs", sprintf("probabilities add to %s, not 1; they'll be rescaled.", sum(probs)))))
    }
    if (has_field(project, g$field) && !is.null(probs)) {
      codes <- field_info(project, g$field)$choices[[1]]$code
      bad <- setdiff(names(probs), codes)
      if (length(bad) > 0) {
        out <- c(out, list(issue("error", "design.groups.probs",
                                 sprintf("codes %s aren't choices of '%s'.", paste(bad, collapse = ", "), g$field),
                                 paste("Choices are:", paste(codes, collapse = ", ")))))
      }
    }
    if (!is.null(g$event)) out <- c(out, list(missing_event(project, g$event, "design.groups.event")))
  }
  dplyr::bind_rows(out)
}

check_covariates <- function(spec, project) {
  purrr::imap(spec$covariates, \(def, name) {
    path <- paste0("covariates.", name)
    dist_ok <- isTRUE(def$dist %in% c("normal", "uniform", "binary", "categorical"))
    dplyr::bind_rows(
      if (!dist_ok) issue("error", path, sprintf("dist '%s' isn't one of normal, uniform, binary, categorical.", def$dist %||% "")),
      if (identical(def$dist, "categorical") && has_field(project, name)) {
        bad <- setdiff(names(unlist(def$probs)), field_info(project, name)$choices[[1]]$code)
        if (length(bad) > 0) issue("error", path, sprintf("codes %s aren't choices of field '%s'.", paste(bad, collapse = ", "), name))
      },
      if (identical(def$dist, "binary")) check_prob(def$prob, paste0(path, ".prob")),
      if (!has_field(project, name)) issue("info", path, "isn't a REDCap field, so it's only used in formulas.")
    )
  }) |>
    dplyr::bind_rows()
}

# Measurement -------------------------------------------------------------

check_measurement <- function(spec, project) {
  all_items <- character()
  out <- purrr::imap(spec$measurement, \(block, name) {
    path <- paste0("measurement.", name)
    if (is.null(block$form) || !has_form(project, block$form)) {
      return(issue("error", paste0(path, ".form"), sprintf("form '%s' isn't in the project.", block$form %||% ""),
                   paste("Forms are:", paste(unique(project$fields$form_name), collapse = ", "))))
    }
    model <- tryCatch(parse_measurement(block), error = \(e) e)
    if (inherits(model, "error")) {
      return(issue("error", paste0(path, ".model"), paste("lavaan couldn't parse the model:", conditionMessage(model)),
                   "Write loadings like: dep =~ 0.7*item1 + 0.6*item2"))
    }
    issues <- list()
    add <- function(...) issues <<- c(issues, list(issue(...)))

    if (!block$form %in% project$mappings$form) {
      add("warning", paste0(path, ".form"), sprintf("form '%s' isn't collected at any event.", block$form))
    }
    for (item in model$unfixed) {
      add("error", paste0(path, ".model"), sprintf("the loading for '%s' has no value.", item),
          sprintf("Fix it to a number, like 0.7*%s. studysynth simulates; it doesn't estimate.", item))
    }
    for (item in intersect(model$items, all_items)) {
      add("error", paste0(path, ".model"), sprintf("item '%s' is already in another measurement block.", item))
    }
    all_items <<- c(all_items, model$items)

    for (item in model$items) {
      info <- field_info(project, item)
      if (nrow(info) == 0) {
        issues <- c(issues, list(missing_field(project, item, paste0(path, ".model"))))
        next
      }
      if (info$form_name != block$form) {
        add("error", paste0(path, ".model"), sprintf("item '%s' is on form '%s', not '%s'.", item, info$form_name, block$form))
      }
      if (!info$field_type %in% c("radio", "dropdown", "yesno", "truefalse")) {
        add("error", paste0(path, ".model"), sprintf("item '%s' is a %s field; items must have choices (radio, dropdown, yesno).", item, info$field_type))
        next
      }
      n_choices <- nrow(info$choices[[1]])
      cuts <- item_thresholds(block, item, n_choices)
      if (length(cuts) != n_choices - 1) {
        add("error", paste0(path, ".thresholds"),
            sprintf("item '%s' has %d choices, so it needs %d thresholds, not %d.", item, n_choices, n_choices - 1, length(cuts)),
            sprintf("Give '%s' its own thresholds, or change the default.", item))
      } else if (is.unsorted(unlist(cuts), strictly = TRUE)) {
        add("error", paste0(path, ".thresholds"), sprintf("thresholds for '%s' must increase.", item))
      }
    }
    for (key in setdiff(names(block$thresholds), c("default", model$items))) {
      add("warning", paste0(path, ".thresholds.", key), "isn't an item in this model.")
    }

    communality <- rowSums(model$lambda^2, na.rm = TRUE)
    for (item in names(communality)[communality >= 1]) {
      add("error", paste0(path, ".model"), sprintf("loadings for '%s' are too big (squared loadings add to %.2f).", item, communality[item]),
          "Loadings are standardized, so squared loadings must add to less than 1.")
    }
    weak <- model$lambda[abs(model$lambda) > 0 & abs(model$lambda) < 0.3]
    if (length(weak) > 0) add("warning", paste0(path, ".model"), "some loadings are below 0.3; those items will be mostly noise.")
    if (all(communality < 1) && min(eigen(model$theta, only.values = TRUE)$values) <= 0) {
      add("error", paste0(path, ".model"), "the residual covariances are impossible (not positive definite).",
          "Make the ~~ values smaller.")
    }
    if (nrow(model$factor_covs) > 0) {
      add("warning", paste0(path, ".model"), "factor covariances (~~ between factors) are ignored.",
          "Correlate latents in latents: instead, e.g. stress ~ dep + ...")
    }
    for (factor in setdiff(model$factors, latent_names(spec))) {
      add("error", path, sprintf("latent '%s' has no block in latents:, so I don't know its values.", factor),
          sprintf("Add latents: %s: {formula: %s ~ 1 + (1 | record_id), fixef: {\"(Intercept)\": 0}, ranef: {record_id: {sd: 1}}, sigma: 0.5}", factor, factor))
    }
    dplyr::bind_rows(issues)
  })
  dplyr::bind_rows(out)
}

# Latents -----------------------------------------------------------------

# A few fake occasions with every variable a formula might use, so we can
# build design matrices without any data.
skeleton_occasions <- function(spec, upto) {
  n <- 12
  data <- tibble::tibble(
    record_id = rep(as.character(1:4), each = 3),
    day = rep(c(0, 1, 30), 4), time = day / 7, hour = stats::runif(n, 9, 21),
    sin_tod = sin(hour), cos_tod = cos(hour), weekend = rep(c(0, 1, 0), 4), ema = rep(c(0, 1, 1), 4)
  )
  for (name in names(spec$covariates)) {
    data[[name]] <- draw_covariate(spec$covariates[[name]], n)
  }
  g <- spec$design$groups
  if (!is.null(g)) {
    data[[g$field]] <- factor(rep_len(names(unlist(g$probs)), n), levels = names(unlist(g$probs)))
  }
  for (name in upto) data[[name]] <- stats::rnorm(n)
  data
}

check_latents <- function(spec, project) {
  earlier <- character()
  out <- purrr::imap(spec$latents, \(block, name) {
    path <- paste0("latents.", name)
    on.exit(earlier <<- c(earlier, name))
    formula <- tryCatch(stats::as.formula(block$formula), error = \(e) e)
    if (inherits(formula, "error") || is.null(block$formula)) {
      return(issue("error", paste0(path, ".formula"), "isn't a formula R can read.",
                   sprintf("Something like: %s ~ time + (1 | record_id)", name)))
    }
    issues <- list()
    add <- function(...) issues <<- c(issues, list(issue(...)))

    if (length(formula) == 3 && deparse(formula[[2]]) != name) {
      add("warning", paste0(path, ".formula"), sprintf("the left side is '%s' but the block is named '%s'.", deparse(formula[[2]]), name))
    }
    data <- skeleton_occasions(spec, earlier)
    unknown <- setdiff(all.vars(formula[[length(formula)]]), names(data))
    if (length(unknown) > 0) {
      later <- intersect(unknown, latent_names(spec))
      add("error", paste0(path, ".formula"), sprintf("doesn't know the variable(s) %s.", paste(unknown, collapse = ", ")),
          if (length(later) > 0) "A latent can only use latents listed above it." else
            paste("Available:", paste(setdiff(names(data), "record_id"), collapse = ", "), "(plus covariates and latents above this one)"))
      return(dplyr::bind_rows(issues))
    }

    x <- fixed_design(formula, data)
    fixef <- unlist(block$fixef)
    for (coef in setdiff(colnames(x), names(fixef))) {
      add("error", paste0(path, ".fixef"), sprintf("needs a value for '%s'.", coef),
          paste("The formula's coefficients are:", paste(colnames(x), collapse = ", ")))
    }
    for (coef in setdiff(names(fixef), colnames(x))) {
      add("error", paste0(path, ".fixef.", coef), "isn't a coefficient of the formula.",
          paste("The formula's coefficients are:", paste(colnames(x), collapse = ", ")))
    }

    bars <- lme4::findbars(formula)
    bar_groups <- purrr::map_chr(bars, \(bar) deparse(bar[[3]]))
    for (bar in bars) {
      group <- deparse(bar[[3]])
      re <- block$ranef[[group]]
      re_path <- paste0(path, ".ranef.", group)
      if (is.null(re)) {
        add("error", paste0(path, ".ranef"), sprintf("the formula has (... | %s) but ranef has no '%s' entry.", group, group))
        next
      }
      terms <- colnames(stats::model.matrix(stats::as.formula(paste("~", deparse(bar[[2]]))), data))
      sd <- unlist(re$sd)
      if (!is.null(names(sd)) && !setequal(names(sd), terms)) {
        add("error", paste0(re_path, ".sd"), sprintf("should name exactly these terms: %s.", paste(terms, collapse = ", ")))
        next
      }
      if (is.null(names(sd)) && length(sd) != length(terms)) {
        add("error", paste0(re_path, ".sd"), sprintf("needs %d values (one per term: %s).", length(terms), paste(terms, collapse = ", ")))
        next
      }
      if (any(sd < 0)) add("error", paste0(re_path, ".sd"), "standard deviations can't be negative.")
      covariance <- tryCatch(ranef_covariance(re, terms), error = \(e) NULL)
      if (is.null(covariance) || any(dim(covariance) != length(terms))) {
        add("error", paste0(re_path, ".cor"), sprintf("should be a %dx%d correlation matrix (or one number for two terms).", length(terms), length(terms)))
      } else if (min(eigen(covariance, only.values = TRUE)$values) < -1e-8) {
        add("error", paste0(re_path, ".cor"), "isn't a possible correlation matrix (not positive definite).")
      }
    }
    for (group in setdiff(names(block$ranef), bar_groups)) {
      add("warning", paste0(path, ".ranef.", group), "isn't a grouping term in the formula, so it's ignored.")
    }
    if (is.null(block$sigma) || !is.numeric(block$sigma) || block$sigma < 0) {
      add("error", paste0(path, ".sigma"), "needs a residual standard deviation (a number, 0 or more).")
    }
    if (!is.null(block$autocor_per_hour) && !(is_prob(block$autocor_per_hour) && block$autocor_per_hour < 1)) {
      add("error", paste0(path, ".autocor_per_hour"), "must be between 0 and 1 (not 1).")
    }
    dplyr::bind_rows(issues)
  })
  dplyr::bind_rows(out)
}

# Experience sampling -----------------------------------------------------

check_ema <- function(spec, project) {
  purrr::imap(spec$ema, \(block, name) {
    path <- paste0("ema.", name)
    event_issue <- missing_event(project, block$event, paste0(path, ".event"))
    if (!is.null(event_issue)) return(event_issue)
    issues <- list()
    add <- function(...) issues <<- c(issues, list(issue(...)))

    if (!is.null(block$form)) {
      if (!block$form %in% forms_at_event(project, block$event)) {
        add("error", paste0(path, ".form"), sprintf("form '%s' isn't collected at '%s'.", block$form, block$event))
      } else if (!is_repeating_form(project, block$form, block$event)) {
        add("error", paste0(path, ".form"), sprintf("form '%s' isn't set up as a repeating instrument at '%s'.", block$form, block$event),
            "Turn on Repeating Instruments in REDCap, or include the repeating CSV.")
      }
    } else if (!any(project$repeating$unique_event_name == block$event & is.na(project$repeating$form_name))) {
      add("error", path, sprintf("event '%s' isn't a repeating event, and no form: is given.", block$event),
          "Name the repeating instrument with form:, or make the event repeat in REDCap.")
    }
    ts <- block$timestamp_field
    if (!is.null(ts)) {
      if (!has_field(project, ts)) {
        issues <- c(issues, list(missing_field(project, ts, paste0(path, ".timestamp_field"))))
      } else if (!field_info(project, ts)$form_name %in% prompt_forms(block, project)) {
        add("error", paste0(path, ".timestamp_field"), sprintf("'%s' isn't on the prompt's form.", ts))
      }
    }
    days <- block$days
    if (is.list(days) && !is.null(days$min) && !is.null(days$max) && days$min > days$max) {
      add("error", paste0(path, ".days"), "min is bigger than max.")
    }
    window <- block$window %||% c("09:00", "21:00")
    if (length(window) != 2 || !all(grepl("^\\d{1,2}:\\d{2}$", window)) || window[1] >= window[2]) {
      add("error", paste0(path, ".window"), "should be two times like [\"09:00\", \"21:00\"], start first.")
    }
    comp <- block$compliance
    if (!is.null(comp$typical) && !(is_prob(comp$typical) && comp$typical > 0 && comp$typical < 1)) {
      add("error", paste0(path, ".compliance.typical"), "must be between 0 and 1 (not exactly 0 or 1).")
    }
    for (latent in setdiff(names(comp$depends_on), latent_names(spec))) {
      add("error", paste0(path, ".compliance.depends_on.", latent), "isn't a latent in latents:.")
    }
    dplyr::bind_rows(issues)
  }) |>
    dplyr::bind_rows()
}

# Screening ---------------------------------------------------------------

check_screening <- function(spec, project) {
  s <- spec$screening
  if (is.null(s)) return(NULL)
  event_issue <- missing_event(project, s$event, "screening.event")
  if (!is.null(event_issue)) return(event_issue)
  out <- list(check_prob(s$decline_prob, "screening.decline_prob"))
  if (!is.null(s$eligible_if)) {
    parsed <- tryCatch(redcap_logic_to_r(s$eligible_if), error = \(e) e)
    if (inherits(parsed, "error")) {
      out <- c(out, list(issue("error", "screening.eligible_if", "isn't REDCap logic I can read.",
                               "Write it like you would in REDCap: [age] >= 18 and [phq9_total] >= 10")))
    }
    screen_fields <- form_fields_at(project, s$event)
    for (field in logic_fields(s$eligible_if)) {
      if (!has_field(project, field)) {
        out <- c(out, list(missing_field(project, field, "screening.eligible_if")))
      } else if (!field %in% screen_fields) {
        out <- c(out, list(issue("error", "screening.eligible_if", sprintf("'%s' isn't collected at '%s'.", field, s$event))))
      }
    }
  }
  if (!is.null(s$consent_field)) {
    out <- c(out, list(missing_field(project, s$consent_field, "screening.consent_field")))
  }
  dplyr::bind_rows(out)
}

form_fields_at <- function(project, event) {
  project$fields$field_name[project$fields$form_name %in% forms_at_event(project, event)]
}

# Missingness -------------------------------------------------------------

check_missingness <- function(spec, project) {
  m <- spec$missingness
  if (is.null(m)) return(NULL)
  out <- list(
    check_prob(m$withdrawal$per_event, "missingness.withdrawal.per_event"),
    check_prob(m$missed_visit, "missingness.missed_visit"),
    check_prob(m$abandon$prob, "missingness.abandon.prob"),
    check_prob(m$skip_item$prob, "missingness.skip_item.prob")
  )
  if (!is.null(m$withdrawal) && is.null(m$withdrawal$per_event)) {
    out <- c(out, list(issue("error", "missingness.withdrawal.per_event", "is required (chance of dropping out at each follow-up).")))
  }
  for (latent in setdiff(names(m$withdrawal$depends_on), latent_names(spec))) {
    out <- c(out, list(issue("error", paste0("missingness.withdrawal.depends_on.", latent), "isn't a latent in latents:.")))
  }
  for (form in names(m$abandon$by_form)) {
    path <- paste0("missingness.abandon.by_form.", form)
    out <- c(out, list(
      check_prob(m$abandon$by_form[[form]], path),
      if (!has_form(project, form)) issue("error", path, sprintf("form '%s' isn't in the project.", form))
    ))
  }
  items <- unlist(purrr::map(spec$measurement, \(block) tryCatch(parse_measurement(block)$items, error = \(e) NULL)))
  for (item in names(m$skip_item$by_item)) {
    path <- paste0("missingness.skip_item.by_item.", item)
    out <- c(out, list(
      check_prob(m$skip_item$by_item[[item]], path),
      missing_field(project, item, path),
      if (has_field(project, item) && !item %in% items) issue("warning", path, "only items in measurement models get skipped; this does nothing.")
    ))
  }
  dplyr::bind_rows(out)
}

check_open_text <- function(spec, project) {
  purrr::imap(spec$open_text, \(def, field) {
    path <- paste0("open_text.", field)
    if (!has_field(project, field)) return(missing_field(project, field, path))
    type <- field_info(project, field)$field_type
    dplyr::bind_rows(
      if (type != "notes") issue("warning", path, sprintf("'%s' is a %s field; only notes fields get open text.", field, type)),
      check_prob(def$skip_prob, paste0(path, ".skip_prob")),
      if (!is.null(def$provider) && is.null(def$model)) issue("warning", path, "has a provider but no model; ellmer will pick its default.")
    )
  }) |>
    dplyr::bind_rows()
}

# Provenance and coverage -------------------------------------------------

check_sources <- function(spec) {
  blocks <- c(
    purrr::imap(spec$measurement, \(b, name) list(path = paste0("measurement.", name), source = b$source)),
    purrr::imap(spec$latents, \(b, name) list(path = paste0("latents.", name), source = b$source))
  )
  purrr::map(blocks, \(b) {
    if (is.null(b$source)) {
      issue("info", b$path, "has no source:. Where did these numbers come from?",
            "Add source: \"Author Year, Table 2\" (or \"researcher\" if you chose them).")
    } else if (identical(b$source, "default")) {
      issue("warning", b$path, "still has placeholder numbers (source: default).",
            "Replace them with values from the scale's papers, then update source:.")
    }
  }) |>
    dplyr::bind_rows()
}

check_coverage <- function(spec, project) {
  covered <- c(
    project$fields$field_name[1],
    unlist(purrr::map(spec$measurement, \(block) tryCatch(parse_measurement(block)$items, error = \(e) NULL))),
    names(spec$covariates), spec$design$groups$field,
    unlist(purrr::map(spec$ema, "timestamp_field")), names(spec$open_text),
    calc_fields(project)
  )
  project$fields |>
    dplyr::filter(!field_name %in% covered, !field_type %in% c("descriptive", "file", "sql")) |>
    dplyr::summarise(fields = paste(field_name, collapse = ", "), n = dplyr::n(), .by = form_name) |>
    purrr::pmap(\(form_name, fields, n) {
      issue("info", paste0("form ", form_name), sprintf("%d field%s type-based defaults: %s", n, if (n == 1) " gets" else "s get", fields))
    }) |>
    dplyr::bind_rows()
}

# Running it --------------------------------------------------------------

check_smoke <- function(spec) {
  result <- tryCatch(
    synthesize(spec, n = 20, check = FALSE, quiet = TRUE),
    error = \(e) e
  )
  if (inherits(result, "error")) {
    issue("error", "(smoke test)", paste("synthesizing 20 people failed:", conditionMessage(result)))
  }
}

check_expectations <- function(spec, project) {
  blocks <- purrr::keep(spec$measurement, \(b) !is.null(b$expect))
  if (length(blocks) == 0) return(NULL)
  data <- synthesize(spec, n = 500, check = FALSE, quiet = TRUE) |> score_scales()
  id <- project$fields$field_name[1]

  purrr::imap(blocks, \(block, name) {
    purrr::imap(block$expect, \(e, stat) {
      path <- paste0("measurement.", name, ".expect.", stat)
      rows <- if (is.null(e$event)) rep(TRUE, nrow(data)) else data$redcap_event_name %in% e$event
      items <- parse_measurement(block)$items
      got <- switch(stat,
        alpha = cronbach_alpha(data[rows, items], parse_measurement(block)$lambda),
        mean_total = mean(data[[paste0(name, "_score")]][rows], na.rm = TRUE),
        sd_total = stats::sd(data[[paste0(name, "_score")]][rows], na.rm = TRUE),
        NA_real_
      )
      if (is.na(got)) return(issue("warning", path, sprintf("I don't know how to check '%s'. Try alpha, mean_total, or sd_total.", stat)))
      off <- abs(got - e$value) > (e$tol %||% 0)
      issue(if (off) "warning" else "info", path,
            sprintf("simulated %.2f, expected %.2f +/- %.2f%s", got, e$value, e$tol %||% 0, if (off) "" else ". Nice."),
            if (off) "Adjust loadings, thresholds, or the latent's intercept and spread." else NA_character_)
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()
}

cronbach_alpha <- function(items, lambda) {
  x <- as.matrix(dplyr::mutate(items, dplyr::across(dplyr::everything(), as.numeric)))
  x <- x[stats::complete.cases(x), , drop = FALSE]
  # Reverse-keyed items count negatively.
  x <- sweep(x, 2, sign(rowSums(lambda[colnames(x), , drop = FALSE])), `*`)
  k <- ncol(x)
  k / (k - 1) * (1 - sum(apply(x, 2, stats::var)) / stats::var(rowSums(x)))
}
