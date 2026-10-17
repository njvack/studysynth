#' @keywords internal
"_PACKAGE"

#' @importFrom rlang %||% .env :=
NULL

# Column names used inside dplyr verbs.
utils::globalVariables(c(
  "arm_num", "branching", "cb", "choice_code", "choice_key", "choices", "choices_raw",
  "code_list", "datetime", "day", "day0", "day_offset", "ema", "ema_block", "event_name",
  "checkbox_choices", "field_label", "field_name", "field_type", "form", "form_name", "level", "lhs", "n_days",
  "occasion_id", "op", "prompt_day", "record_id", "redcap_event_name",
  "redcap_repeat_instance", "redcap_repeat_instrument", "rhs", "slot", "unique_event_name",
  "validation", "status", "group", "value", "label", "event", "total", "response", "item"
))
