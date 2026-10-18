#' Read a REDCap project's structure
#'
#' Reads the *structure* of a REDCap project, never its records. Give it
#' either the metadata-only project XML ("Download metadata only (XML)" in
#' REDCap's Other Functions page), or the data dictionary CSV plus the events,
#' instrument designations, and (optionally) repeating instruments CSVs.
#'
#' A project XML that contains records (a `ClinicalData` section) is refused.
#'
#' @param xml Path to a metadata-only REDCap project XML file.
#' @param dictionary Path to the data dictionary CSV.
#' @param events Path to the events CSV. Omit for classic (non-longitudinal)
#'   projects.
#' @param mappings Path to the instrument designations CSV.
#' @param repeating Path to the repeating instruments and events CSV.
#' @return A `redcap_project`: a list of tibbles named `fields`, `events`,
#'   `mappings`, and `repeating`.
#' @export
#' @examples
#' project <- read_redcap_project(example_path("CalmMinds_metadata.xml"))
#' project
read_redcap_project <- function(xml = NULL, dictionary = NULL, events = NULL,
                                mappings = NULL, repeating = NULL) {
  if (!is.null(xml)) {
    return(read_project_xml(xml))
  }
  if (is.null(dictionary)) {
    cli::cli_abort("Give me either {.arg xml} or {.arg dictionary}.")
  }

  fields <- read_dictionary(dictionary)
  if (is.null(events)) {
    # Classic project: pretend there is one event that collects every form.
    events <- tibble::tibble(
      unique_event_name = "event_1_arm_1", event_name = "Event 1",
      arm_num = 1L, day_offset = 0
    )
    mappings <- tibble::tibble(
      arm_num = 1L, unique_event_name = "event_1_arm_1",
      form = unique(fields$form_name)
    )
  } else {
    events <- readr::read_csv(events, col_types = readr::cols(.default = "c")) |>
      dplyr::transmute(
        unique_event_name, event_name,
        arm_num = as.integer(arm_num), day_offset = as.numeric(day_offset)
      )
    mappings <- readr::read_csv(mappings, col_types = readr::cols(.default = "c")) |>
      dplyr::transmute(arm_num = as.integer(arm_num), unique_event_name, form)
  }
  repeating <- if (is.null(repeating)) {
    empty_repeating()
  } else {
    readr::read_csv(repeating, col_types = readr::cols(.default = "c")) |>
      dplyr::transmute(unique_event_name = event_name, form_name = dplyr::na_if(form_name, ""))
  }

  new_redcap_project(fields, events, mappings, repeating)
}

new_redcap_project <- function(fields, events, mappings, repeating) {
  structure(
    list(fields = fields, events = events, mappings = mappings, repeating = repeating),
    class = "redcap_project"
  )
}

empty_repeating <- function() {
  tibble::tibble(unique_event_name = character(), form_name = character())
}

#' @export
print.redcap_project <- function(x, ...) {
  cli::cli_text("{.cls redcap_project}")
  cli::cli_bullets(c(
    "*" = "{nrow(x$fields)} fields on {length(unique(x$fields$form_name))} forms",
    "*" = "{nrow(x$events)} events: {.val {x$events$unique_event_name}}",
    "*" = "{nrow(x$repeating)} repeating instrument{?s}/event{?s}"
  ))
  invisible(x)
}

# Data dictionary ---------------------------------------------------------

read_dictionary <- function(path) {
  raw <- readr::read_csv(path, col_types = readr::cols(.default = "c"), na = "")
  # Column names vary a little between REDCap versions; match on prefixes.
  col <- function(prefix) {
    hit <- names(raw)[stringr::str_starts(names(raw), stringr::fixed(prefix))]
    if (length(hit) == 0) rep(NA_character_, nrow(raw)) else raw[[hit[1]]]
  }
  tibble::tibble(
    field_name = col("Variable / Field Name"),
    form_name = col("Form Name"),
    field_type = col("Field Type"),
    field_label = col("Field Label"),
    choices_raw = col("Choices, Calculations"),
    validation = col("Text Validation Type"),
    val_min = col("Text Validation Min"),
    val_max = col("Text Validation Max"),
    branching = col("Branching Logic")
  ) |>
    finish_fields()
}

# Turns choice strings into tidy tibbles and pulls out calc expressions.
finish_fields <- function(fields) {
  fields |>
    dplyr::mutate(
      calc = dplyr::if_else(field_type == "calc", choices_raw, NA_character_),
      choices = purrr::map2(choices_raw, field_type, parse_choices)
    ) |>
    dplyr::select(dplyr::all_of(field_columns_order))
}

field_columns_order <- c("field_name", "form_name", "field_type", "field_label", "validation",
                         "val_min", "val_max", "branching", "calc", "choices")

#' Parse a REDCap choices string
#'
#' @param x A string like `"1, Never | 2, Sometimes"`.
#' @param field_type The REDCap field type. `yesno` and `truefalse` fields
#'   have implied choices.
#' @return A tibble with `code` and `label` columns.
#' @export
#' @examples
#' parse_choices("0, Not at all | 1, Several days")
parse_choices <- function(x, field_type = "radio") {
  if (identical(field_type, "yesno")) {
    return(tibble::tibble(code = c("0", "1"), label = c("No", "Yes")))
  }
  if (identical(field_type, "truefalse")) {
    return(tibble::tibble(code = c("0", "1"), label = c("False", "True")))
  }
  if (!field_type %in% c("radio", "dropdown", "checkbox") || is.na(x) || x == "") {
    return(tibble::tibble(code = character(), label = character()))
  }
  pieces <- stringr::str_split_1(x, stringr::fixed("|")) |> stringr::str_trim()
  tibble::tibble(
    code = stringr::str_trim(stringr::str_extract(pieces, "^[^,]+")),
    label = stringr::str_trim(stringr::str_remove(pieces, "^[^,]+,"))
  )
}

# Project XML -------------------------------------------------------------

read_project_xml <- function(path) {
  doc <- xml2::read_xml(path)
  xml2::xml_ns_strip(doc)

  if (length(xml2::xml_find_all(doc, "//ClinicalData")) > 0) {
    cli::cli_abort(c(
      "This project XML contains records ({.code ClinicalData}), so I won't read it.",
      "i" = "studysynth only ever looks at a project's structure, never at subject data.",
      ">" = "In REDCap, use {.strong Other Functions > Download metadata only (XML)} instead."
    ))
  }

  # After stripping the default namespace, REDCap attributes keep their
  # "redcap:" prefix; this helper hides that detail.
  attr_of <- function(nodes, name) {
    out <- xml2::xml_attr(nodes, name)
    if (all(is.na(out))) out <- xml2::xml_attr(nodes, paste0("redcap:", name))
    out
  }

  item_defs <- xml2::xml_find_all(doc, "//ItemDef")
  items <- tibble::tibble(
    oid = xml2::xml_attr(item_defs, "OID"),
    field_name = attr_of(item_defs, "Variable"),
    field_type = attr_of(item_defs, "FieldType"),
    field_label = purrr::map_chr(item_defs, \(node) {
      xml2::xml_text(xml2::xml_find_first(node, "./Question/TranslatedText"))
    }),
    validation = attr_of(item_defs, "TextValidationType"),
    branching = attr_of(item_defs, "BranchingLogic"),
    calc = attr_of(item_defs, "Calculation"),
    checkbox_choices = attr_of(item_defs, "CheckboxChoices"),
    code_list = purrr::map_chr(item_defs, \(node) {
      xml2::xml_attr(xml2::xml_find_first(node, "./CodeListRef"), "CodeListOID")
    }),
    val_min = purrr::map_chr(item_defs, \(node) range_check(node, "GE")),
    val_max = purrr::map_chr(item_defs, \(node) range_check(node, "LE"))
  ) |>
    dplyr::filter(field_type != "select" | !stringr::str_ends(field_name, "_complete"))

  # Which form each item belongs to, via FormDef -> ItemGroupRef -> ItemRef.
  item_forms <- purrr::map_dfr(xml2::xml_find_all(doc, "//FormDef"), \(form) {
    group_oids <- xml2::xml_attr(xml2::xml_find_all(form, "./ItemGroupRef"), "ItemGroupOID")
    item_oids <- purrr::map(group_oids, \(oid) {
      group <- xml2::xml_find_first(doc, sprintf("//ItemGroupDef[@OID='%s']", oid))
      xml2::xml_attr(xml2::xml_find_all(group, "./ItemRef"), "ItemOID")
    }) |> unlist()
    tibble::tibble(oid = item_oids, form_name = attr_of(form, "FormName"))
  })

  code_lists <- xml2::xml_find_all(doc, "//CodeList")
  choices_by_list <- purrr::set_names(
    purrr::map(code_lists, \(node) {
      code_items <- xml2::xml_find_all(node, "./CodeListItem")
      tibble::tibble(
        code = xml2::xml_attr(code_items, "CodedValue"),
        label = xml2::xml_text(xml2::xml_find_first(code_items, "./Decode/TranslatedText"))
      )
    }),
    xml2::xml_attr(code_lists, "OID")
  )

  fields <- items |>
    dplyr::inner_join(item_forms, by = "oid") |>
    dplyr::mutate(
      # Checkbox fields appear once per choice (race___1, race___2, ...).
      choice_code = stringr::str_match(oid, "___(.+)$")[, 2],
      choices = purrr::map(code_list, \(oid) choices_by_list[[oid]] %||% parse_choices(NA))
    )
  # Choice labels come from redcap:CheckboxChoices when present; otherwise
  # all we know are the codes.
  checkbox_choices <- fields |>
    dplyr::filter(field_type == "checkbox") |>
    dplyr::summarise(
      cb = list(if (!is.na(checkbox_choices[1])) {
        parse_choices(checkbox_choices[1], "checkbox")
      } else {
        tibble::tibble(code = choice_code, label = choice_code)
      }),
      .by = field_name
    )
  fields <- fields |>
    dplyr::distinct(field_name, .keep_all = TRUE) |>
    dplyr::left_join(checkbox_choices, by = "field_name") |>
    dplyr::mutate(
      choices = dplyr::if_else(field_type == "checkbox", cb, choices),
      choices = purrr::map2(choices, field_type, \(ch, type) {
        if (type %in% c("yesno", "truefalse")) parse_choices(NA, type) else ch
      })
    ) |>
    dplyr::select(dplyr::all_of(field_columns_order))

  event_defs <- xml2::xml_find_all(doc, "//StudyEventDef")
  events <- tibble::tibble(
    unique_event_name = attr_of(event_defs, "UniqueEventName"),
    event_name = xml2::xml_attr(event_defs, "Name"),
    arm_num = as.integer(attr_of(event_defs, "ArmNum")),
    day_offset = as.numeric(attr_of(event_defs, "DayOffset"))
  )
  mappings <- purrr::map_dfr(seq_along(event_defs), \(i) {
    refs <- xml2::xml_find_all(event_defs[[i]], "./FormRef")
    tibble::tibble(
      arm_num = events$arm_num[i],
      unique_event_name = events$unique_event_name[i],
      form = attr_of(refs, "FormName")
    )
  })
  if (nrow(events) == 0) {
    events <- tibble::tibble(unique_event_name = "event_1_arm_1", event_name = "Event 1",
                             arm_num = 1L, day_offset = 0)
    mappings <- tibble::tibble(arm_num = 1L, unique_event_name = "event_1_arm_1",
                               form = unique(fields$form_name))
  }

  rep_instruments <- xml2::xml_find_all(doc, "//*[local-name()='RepeatingInstrument']")
  rep_events <- xml2::xml_find_all(doc, "//*[local-name()='RepeatingEvent']")
  repeating <- dplyr::bind_rows(
    tibble::tibble(
      unique_event_name = attr_of(rep_instruments, "UniqueEventName"),
      form_name = attr_of(rep_instruments, "RepeatInstrument")
    ),
    tibble::tibble(unique_event_name = attr_of(rep_events, "UniqueEventName"), form_name = NA_character_)
  )

  new_redcap_project(fields, events, mappings, repeating)
}

range_check <- function(node, comparator) {
  check <- xml2::xml_find_first(node, sprintf("./RangeCheck[@Comparator='%s']/CheckValue", comparator))
  if (inherits(check, "xml_missing")) NA_character_ else xml2::xml_text(check)
}

# Small lookups used everywhere -------------------------------------------

forms_at_event <- function(project, event) {
  project$mappings$form[project$mappings$unique_event_name == event]
}

form_fields <- function(project, form) {
  dplyr::filter(project$fields, form_name == form)
}

field_info <- function(project, field) {
  project$fields[project$fields$field_name == field, ]
}

is_repeating_form <- function(project, form, event) {
  any(project$repeating$unique_event_name == event &
        project$repeating$form_name %in% form)
}

#' Path to a file in the bundled Calm Minds example
#'
#' Calm Minds is an invented mindfulness trial for depression: screening,
#' baseline, a two-week experience sampling burst, three follow-ups.
#'
#' @param file A file name, or `NULL` to list the files.
#' @export
#' @examples
#' example_path()
example_path <- function(file = NULL) {
  dir <- system.file("examples", "calm_minds", package = "studysynth")
  if (is.null(file)) {
    return(list.files(dir))
  }
  file.path(dir, file)
}
