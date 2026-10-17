#' Read and write studysynth spec files
#'
#' A spec is a YAML file describing an imaginary study population: how items
#' relate (`measurement:`, lavaan syntax), how people change (`latents:`,
#' lme4-style formulas), screening, missingness, experience sampling, and
#' open text. See `vignette("spec")` or the example from
#' `example_path("calm_minds.yaml")`.
#'
#' @param path Path to a YAML file.
#' @return A `studysynth_spec`: a list, remembering where it came from so
#'   relative paths inside it work.
#' @export
#' @examples
#' spec <- read_spec(example_path("calm_minds.yaml"))
#' names(spec)
read_spec <- function(path) {
  spec <- yaml::read_yaml(path)
  as_spec(spec, dir = dirname(normalizePath(path)))
}

#' @rdname read_spec
#' @param spec A spec list.
#' @export
write_spec <- function(spec, path) {
  spec <- unclass(spec)
  attr(spec, "dir") <- NULL
  yaml::write_yaml(spec, path)
  invisible(path)
}

#' Parse spec text
#'
#' @param text YAML text.
#' @param dir Directory that relative paths in the spec are relative to.
#' @return A `studysynth_spec`.
#' @export
parse_spec <- function(text, dir = ".") {
  as_spec(yaml::yaml.load(text), dir = dir)
}

as_spec <- function(x, dir = ".") {
  if (inherits(x, "studysynth_spec")) {
    return(x)
  }
  if (is.character(x) && length(x) == 1) {
    return(read_spec(x))
  }
  structure(x, class = "studysynth_spec", dir = dir)
}

#' @export
print.studysynth_spec <- function(x, ...) {
  cli::cli_text("{.cls studysynth_spec} {x$title %||% ''}")
  cli::cli_bullets(c(
    "*" = "{x$design$n_subjects %||% '?'} enrolled participants",
    "*" = "Measurement models: {.val {names(x$measurement)}}",
    "*" = "Latent variables: {.val {names(x$latents)}}"
  ))
  invisible(x)
}

#' Load the REDCap project a spec points at
#'
#' @param spec A spec, or a path to one.
#' @return A `redcap_project`.
#' @export
spec_project <- function(spec) {
  spec <- as_spec(spec)
  rc <- spec$redcap
  if (is.null(rc)) {
    cli::cli_abort("The spec has no {.field redcap:} block saying where the project files are.")
  }
  in_dir <- function(file) {
    if (is.null(file) || grepl("^(/|~|[A-Za-z]:)", file)) file else file.path(attr(spec, "dir") %||% ".", file)
  }
  read_redcap_project(
    xml = in_dir(rc$xml), dictionary = in_dir(rc$dictionary),
    events = in_dir(rc$events), mappings = in_dir(rc$mappings),
    repeating = in_dir(rc$repeating)
  )
}

#' Draft a starter spec from a REDCap project
#'
#' Makes a plain, deliberately boring first draft: every form with three or
#' more items sharing the same choices gets a one-factor model with loadings
#' of 0.6, every factor gets a flat trajectory, and repeating forms get an
#' experience sampling block. Every number is tagged `source: default`, so the
#' checker reminds you (or your LLM, armed with the scale's papers) to
#' replace them.
#'
#' @param project A `redcap_project`.
#' @param redcap The `redcap:` block to write into the spec (paths to the
#'   project files, relative to where the spec will live).
#' @param n_subjects How many participants to enroll.
#' @return A `studysynth_spec`.
#' @export
#' @examples
#' project <- read_redcap_project(example_path("CalmMinds_metadata.xml"))
#' draft <- draft_spec(project, redcap = list(xml = "CalmMinds_metadata.xml"))
#' names(draft$measurement)
draft_spec <- function(project, redcap = list(xml = "project.xml"), n_subjects = 100) {
  scales <- project$fields |>
    dplyr::filter(field_type %in% c("radio", "dropdown")) |>
    dplyr::mutate(choice_key = purrr::map_chr(choices, \(ch) paste(ch$code, ch$label, sep = "=", collapse = ","))) |>
    dplyr::filter(dplyr::n() >= 3, .by = c(form_name, choice_key)) |>
    dplyr::summarise(
      items = list(field_name),
      n_choices = length(strsplit(choice_key[1], ",")[[1]]),
      .by = c(form_name, choice_key)
    )

  measurement <- list()
  latents <- list()
  for (i in seq_len(nrow(scales))) {
    form <- scales$form_name[i]
    factor_name <- if (form %in% names(measurement)) paste0(form, "_", i) else form
    items <- scales$items[[i]]
    measurement[[factor_name]] <- list(
      form = form,
      model = paste0(factor_name, " =~ ", paste0("0.6*", items, collapse = " + ")),
      thresholds = list(default = even_thresholds(scales$n_choices[i])),
      source = "default"
    )
    latents[[factor_name]] <- list(
      formula = paste0(factor_name, " ~ time + (1 | record_id)"),
      fixef = list("(Intercept)" = 0, time = 0),
      ranef = list(record_id = list(sd = 1)),
      sigma = 0.5,
      source = "default"
    )
  }

  ema <- list()
  for (i in seq_len(nrow(project$repeating))) {
    form <- project$repeating$form_name[i]
    if (is.na(form)) next
    ts_field <- form_fields(project, form) |>
      dplyr::filter(validation %in% c("datetime_ymd", "datetime_mdy", "datetime_dmy",
                                      "datetime_seconds_ymd")) |>
      dplyr::pull(field_name)
    ema[[form]] <- list(
      event = project$repeating$unique_event_name[i],
      form = form,
      timestamp_field = if (length(ts_field) > 0) ts_field[1] else NULL,
      days = list(min = 7, max = 14),
      prompts_per_day = 4,
      window = c("09:00", "21:00"),
      compliance = list(typical = 0.75, person_sd = 0.8, per_day = -0.03)
    )
  }

  as_spec(list(
    studysynth = "0.1",
    title = "Draft spec",
    seed = 1,
    redcap = redcap,
    design = list(n_subjects = n_subjects, start_date = format(Sys.Date()),
                  time = list(unit_days = 7)),
    measurement = measurement,
    latents = latents,
    ema = if (length(ema) > 0) ema,
    missingness = list(skip_item = list(prob = 0.01))
  ) |> purrr::compact())
}

# Thresholds that make every category equally likely for an average person.
even_thresholds <- function(n_choices) {
  round(stats::qnorm(seq_len(n_choices - 1) / n_choices), 2)
}
