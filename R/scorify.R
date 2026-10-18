#' Write a scorify scoresheet for your measures
#'
#' Makes a [scorify](https://github.com/njvack/scorify) scoresheet that
#' scores a REDCap export the same way [score_scales()] does: one sum per
#' scale or subscale, reverse-scoring items with negative loadings. Run it
#' on your fake data now and your real export later, and check the two
#' agree on what a score means.
#'
#' @param spec A spec, or a path to one.
#' @param path Where to write the scoresheet (CSV).
#' @return The scoresheet as a tibble, invisibly.
#' @export
#' @examples
#' sheet <- write_scorify_scoresheet(example_path("calm_minds.yaml"), tempfile(fileext = ".csv"))
#' head(sheet, 12)
write_scorify_scoresheet <- function(spec, path) {
  spec <- as_spec(spec)
  project <- spec_project(spec)
  id_field <- project$fields$field_name[1]

  # Each subscale's items, with their response codes and direction.
  items <- purrr::imap(spec$measurement, \(block, name) {
    purrr::map(subscales(name, block), \(sub) {
      codes <- purrr::map(sub$items, \(item) as.numeric(field_info(project, item)$choices[[1]]$code))
      tibble::tibble(
        item = sub$items,
        measure = sub$name,
        low = purrr::map_dbl(codes, min),
        high = purrr::map_dbl(codes, max),
        reversed = unname(sub$sign < 0)
      )
    })
  }) |>
    purrr::list_flatten() |>
    dplyr::bind_rows() |>
    dplyr::mutate(transform = ifelse(reversed, sprintf("reverse_%g_%g", low, high), ""))

  reversals <- dplyr::distinct(dplyr::filter(items, reversed), transform, low, high)
  key_columns <- c(id_field, "redcap_event_name", if (nrow(project$repeating) > 0)
    c("redcap_repeat_instrument", "redcap_repeat_instance"))
  measures <- unique(items$measure)

  row <- function(a, b = "", c = "", d = "") tibble::tibble(a, b, c, d)
  sheet <- dplyr::bind_rows(
    row("layout", c("header", "data")),
    row(""),
    row("transform", reversals$transform, sprintf("map(%g:%g,%g:%g)", reversals$low, reversals$high, reversals$high, reversals$low)),
    row(""),
    row("score", key_columns),
    row("score", items$item, items$measure, items$transform),
    row(""),
    row("measure", paste0(measures, "_score"), sprintf("sum(%s)", measures))
  )
  readr::write_csv(sheet, path, col_names = FALSE, na = "")
  invisible(sheet)
}
