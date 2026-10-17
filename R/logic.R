#' Translate REDCap logic into an R expression
#'
#' REDCap logic (branching logic, calculated fields, eligibility rules) looks
#' like `[age] >= 18 and [phq9_total] >= 10`. This turns it into R code that
#' works on whole columns at once. It handles the common cases: field
#' references, checkbox references like `[race(2)]`, `=`, `<>`, `and`, `or`,
#' `not`, and the functions `sum`, `mean`, `min`, `max`, `round`, `abs`, `if`.
#' Cross-event references like `[baseline_arm_1][age]` use the current row.
#'
#' @param logic A REDCap logic string.
#' @return An R expression.
#' @export
#' @examples
#' redcap_logic_to_r("[age] >= 18 and [consent_given] = '1'")
redcap_logic_to_r <- function(logic) {
  x <- logic |>
    # [event][field] -> [field]: we evaluate within a row, so drop the event.
    stringr::str_replace_all("\\[[A-Za-z0-9_]+\\](?=\\[)", "") |>
    # Checkbox [field(code)] -> field___code
    stringr::str_replace_all("\\[([A-Za-z0-9_]+)\\(([A-Za-z0-9_-]+)\\)\\]", "`\\1___\\2`") |>
    stringr::str_replace_all("\\[([A-Za-z0-9_]+)\\]", "`\\1`") |>
    stringr::str_replace_all("<>", " != ") |>
    stringr::str_replace_all("(?<![<>!=])=(?!=)", " == ") |>
    stringr::str_replace_all("(?i)\\band\\b", " & ") |>
    stringr::str_replace_all("(?i)\\bor\\b", " | ") |>
    stringr::str_replace_all("(?i)\\bnot\\b", " ! ") |>
    stringr::str_replace_all("\\bif\\s*\\(", "rc_if(") |>
    stringr::str_replace_all("\\b(sum|mean|min|max)\\s*\\(", "rc_\\1(")
  parse(text = x)[[1]]
}

#' Evaluate REDCap logic against a data frame
#'
#' Values are compared the way REDCap does: numbers as numbers when both sides
#' look numeric, and blanks (`NA`) never satisfy a comparison.
#'
#' @param logic A REDCap logic string.
#' @param data A data frame with one column per field.
#' @return A vector with one value per row.
#' @export
#' @examples
#' people <- data.frame(age = c(17, 30), consent_given = c(1, 1))
#' eval_redcap_logic("[age] >= 18 and [consent_given] = '1'", people)
eval_redcap_logic <- function(logic, data) {
  expr <- redcap_logic_to_r(logic)
  n <- nrow(data)
  # REDCap compares '1' and 1 as equal; numeric-looking text becomes numbers.
  data <- purrr::map(as.list(data)[intersect(all.vars(expr), names(data))], \(col) {
    num <- suppressWarnings(as.numeric(col))
    if (all(is.na(num) == is.na(col))) num else col
  })
  env <- rlang::env(rlang::base_env(), !!!logic_helpers())
  out <- eval(expr, envir = data, enclos = env)
  if (length(out) == 1) out <- rep(out, n)
  out
}

logic_helpers <- function() {
  as_num <- function(x) if (is.character(x)) suppressWarnings(as.numeric(x)) else x
  na_if_all_na <- function(result, args) {
    result[rowSums(!is.na(args)) == 0] <- NA
    result
  }
  bind <- function(...) do.call(cbind, lapply(list(...), as_num))
  list(
    rc_sum = function(...) {
      args <- bind(...)
      na_if_all_na(rowSums(args, na.rm = TRUE), args)
    },
    rc_mean = function(...) {
      args <- bind(...)
      na_if_all_na(rowMeans(args, na.rm = TRUE), args)
    },
    rc_min = function(...) do.call(pmin, c(lapply(list(...), as_num), na.rm = TRUE)),
    rc_max = function(...) do.call(pmax, c(lapply(list(...), as_num), na.rm = TRUE)),
    rc_if = function(test, yes, no) ifelse(test, yes, no)
  )
}

# Which fields a piece of logic refers to.
logic_fields <- function(logic) {
  x <- stringr::str_replace_all(logic, "\\[[A-Za-z0-9_]+\\](?=\\[)", "")
  refs <- stringr::str_match_all(x, "\\[([A-Za-z0-9_]+)(\\([^)]*\\))?\\]")[[1]]
  unique(refs[, 2])
}
