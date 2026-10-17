# Turning occasions into REDCap rows.
#
# A REDCap row is identified by record_id + event + repeat instrument +
# repeat instance. Ordinary visits put all of an event's forms on one row;
# each answered prompt of a repeating instrument gets its own row.

make_records <- function(spec, project, occasions, people) {
  instances <- form_instances(spec, project, occasions)
  pieces <- purrr::map(unique(instances$form), \(form) {
    rows <- dplyr::filter(instances, form == .env$form)
    values <- form_values(spec, project, form, occasions[match(rows$occasion_id, occasions$occasion_id), ], people)
    values[[complete_field(form)]] <- "2"
    dplyr::bind_cols(dplyr::select(rows, dplyr::all_of(row_keys), occasion_id), values)
  })
  purrr::reduce(pieces, \(a, b) dplyr::full_join(a, b, by = c(row_keys, "occasion_id"))) |>
    dplyr::arrange(as.numeric(record_id), occasion_id)
}

row_keys <- c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")

complete_field <- function(form) paste0(form, "_complete")

form_instances <- function(spec, project, occasions) {
  visits <- dplyr::filter(occasions, ema == 0)
  visit_rows <- purrr::map_dfr(unique(visits$unique_event_name), \(event) {
    tidyr::expand_grid(
      dplyr::filter(visits, unique_event_name == event),
      form = visit_forms(spec, project, event)
    )
  }) |>
    dplyr::mutate(redcap_repeat_instrument = NA_character_, redcap_repeat_instance = NA_integer_)

  prompt_rows <- purrr::imap_dfr(spec$ema, \(block, name) {
    occasions |>
      dplyr::filter(ema_block == name) |>
      dplyr::arrange(datetime) |>
      dplyr::mutate(redcap_repeat_instance = dplyr::row_number(), .by = record_id) |>
      tidyr::expand_grid(form = prompt_forms(block, project)) |>
      # A repeating instrument names its form; a repeating event leaves it blank.
      dplyr::mutate(redcap_repeat_instrument = if (is.null(block$form)) NA_character_ else form)
  })

  dplyr::bind_rows(visit_rows, prompt_rows) |>
    dplyr::transmute(
      record_id, redcap_event_name = unique_event_name,
      redcap_repeat_instrument, redcap_repeat_instance, occasion_id, form
    )
}

# All values for one form at a set of occasions, as character columns.
form_values <- function(spec, project, form, occasions, people) {
  fields <- form_fields(project, form) |>
    dplyr::filter(!field_type %in% c("descriptive", "file", "sql"),
                  field_name != project$fields$field_name[1])
  n <- nrow(occasions)

  items <- purrr::map(
    purrr::keep(spec$measurement, \(block) block$form == form),
    \(block) simulate_items(block, project, occasions)
  ) |>
    unname() |>
    dplyr::bind_cols()
  if (length(items) == 0) items <- tibble::tibble(.rows = n)

  timestamp_fields <- unlist(purrr::map(spec$ema, "timestamp_field"))
  group_field <- spec$design$groups$field %||% ""

  values <- purrr::map(seq_len(nrow(fields)), \(i) {
    info <- fields[i, ]
    name <- info$field_name
    if (name %in% names(items)) {
      items[[name]]
    } else if (name == group_field) {
      as.character(occasions[[name]])
    } else if (name %in% names(spec$covariates)) {
      as.character(occasions[[name]])
    } else if (name %in% timestamp_fields) {
      format(occasions$datetime, "%Y-%m-%d %H:%M")
    } else {
      default_values(info, occasions)
    }
  })
  names(values) <- fields$field_name

  # Checkbox fields come back as data frames (one column per choice).
  purrr::imap(values, \(v, name) {
    if (is.data.frame(v)) v else tibble::tibble(!!name := v)
  }) |>
    unname() |>
    dplyr::bind_cols(.name_repair = "minimal")
}

# Plausible-looking values for fields the spec doesn't mention, based only
# on the field's type and validation.
default_values <- function(info, occasions) {
  n <- nrow(occasions)
  codes <- info$choices[[1]]$code
  type <- info$field_type
  validation <- dplyr::coalesce(info$validation, "")
  lo <- suppressWarnings(as.numeric(info$val_min))
  hi <- suppressWarnings(as.numeric(info$val_max))
  lo <- if (is.na(lo)) 0 else lo
  hi <- if (is.na(hi)) 100 else hi
  dates <- as.Date(occasions$datetime)

  if (type %in% c("radio", "dropdown", "yesno", "truefalse")) {
    return(sample(codes, n, replace = TRUE))
  }
  if (type == "checkbox") {
    picked <- sample(codes, n, replace = TRUE)
    cols <- purrr::map(codes, \(code) dplyr::if_else(picked == code, "1", "0"))
    return(tibble::as_tibble(purrr::set_names(cols, paste0(info$field_name, "___", codes))))
  }
  if (type == "slider") {
    return(as.character(round(stats::runif(n, 0, 100))))
  }
  if (type %in% c("calc", "notes")) {
    return(rep(NA_character_, n))
  }
  switch(validation,
    integer = as.character(round(stats::runif(n, lo, hi))),
    number = as.character(round(stats::runif(n, lo, hi), 1)),
    date_ymd = format(dates, "%Y-%m-%d"),
    date_mdy = format(dates, "%m-%d-%Y"),
    date_dmy = format(dates, "%d-%m-%Y"),
    datetime_ymd = , datetime_seconds_ymd = format(occasions$datetime, "%Y-%m-%d %H:%M"),
    time = format(occasions$datetime, "%H:%M"),
    email = paste0("participant", occasions$record_id, "@example.org"),
    phone = sprintf("(555) 555-%04d", sample(100:199, n, replace = TRUE)),
    zipcode = sprintf("%05d", sample(10000:99999, n, replace = TRUE)),
    rep("[synthetic text]", n)
  )
}

# Calculated fields, evaluated row by row in dictionary order.
compute_calcs <- function(project, records) {
  calcs <- dplyr::filter(project$fields, field_type == "calc", field_name %in% names(records))
  for (i in seq_len(nrow(calcs))) {
    has_form <- !is.na(records[[complete_field(calcs$form_name[i])]])
    value <- eval_redcap_logic(calcs$calc[i], records[has_form, ])
    records[[calcs$field_name[i]]][has_form] <- format_number(value)
  }
  records
}

format_number <- function(x) {
  out <- as.character(round(x, 4))
  out[is.na(x)] <- NA_character_
  out
}

# Blank out fields whose branching logic says they wouldn't be shown.
apply_branching <- function(project, records) {
  branched <- dplyr::filter(project$fields, !is.na(branching), branching != "")
  for (i in seq_len(nrow(branched))) {
    columns <- field_columns(branched[i, ], names(records))
    if (length(columns) == 0) next
    shown <- eval_redcap_logic(branched$branching[i], records) %in% TRUE
    records[!shown, columns] <- NA
  }
  records
}

# The output column(s) for a field: checkboxes have one per choice.
field_columns <- function(info, available) {
  columns <- if (info$field_type == "checkbox") {
    paste0(info$field_name, "___", info$choices[[1]]$code)
  } else {
    info$field_name
  }
  intersect(columns, available)
}

form_columns <- function(project, form, available) {
  fields <- form_fields(project, form)
  unlist(purrr::map(seq_len(nrow(fields)), \(i) field_columns(fields[i, ], available)))
}
