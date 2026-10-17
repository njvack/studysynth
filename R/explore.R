#' Who made it through your imaginary study?
#'
#' A CONSORT-style count of screened, ineligible, declined, enrolled,
#' withdrawn, and completed participants.
#'
#' @param data Output of [synthesize()].
#' @return A tibble of counts, invisibly.
#' @export
study_flow <- function(data) {
  status <- attr(data, "status")
  counts <- c(
    screened = nrow(status),
    ineligible = sum(status$status == "ineligible"),
    declined = sum(status$status == "declined"),
    enrolled = sum(status$status %in% enrolled_statuses),
    withdrew = sum(status$status == "withdrew"),
    completed = sum(status$status == "completed")
  )
  withdrew_at <- table(status$withdrawn_at)
  line <- function(label, n, extra = "") cli::cat_line(sprintf("%-16s%5d", label, n), extra)

  cli::cli_text("{.strong Study flow}")
  line("Screened", counts[["screened"]])
  line("|- Ineligible", counts[["ineligible"]])
  line("|- Declined", counts[["declined"]])
  line("`- Enrolled", counts[["enrolled"]])
  line("   |- Withdrew", counts[["withdrew"]],
       if (length(withdrew_at) > 0) paste0("  (", paste(names(withdrew_at), withdrew_at, sep = ": ", collapse = ", "), ")"))
  line("   `- Completed", counts[["completed"]])

  invisible(tibble::enframe(counts, name = "stage", value = "n"))
}

#' The latent values behind the responses
#'
#' Every occasion (visit or prompt) with each person's simulated latent
#' scores. Handy for checking that your analysis recovers what you put in.
#'
#' @param data Output of [synthesize()].
#' @return A tibble.
#' @export
true_scores <- function(data) {
  spec <- attr(data, "spec")
  attr(data, "occasions") |>
    dplyr::select(record_id, event = unique_event_name, datetime, time,
                  dplyr::any_of(spec$design$groups$field), dplyr::all_of(names(spec$latents)))
}

#' Score scales by summing their items
#'
#' Adds one `<block>_score` column per measurement block (the sum of its
#' items, after reverse-scoring items with negative loadings). Rows missing
#' any item get `NA`.
#'
#' @param data Output of [synthesize()].
#' @return `data` with score columns added.
#' @export
score_scales <- function(data) {
  spec <- attr(data, "spec")
  project <- spec_project(spec)
  for (name in names(spec$measurement)) {
    model <- parse_measurement(spec$measurement[[name]])
    scores <- purrr::map(model$items, \(item) {
      codes <- as.numeric(field_info(project, item)$choices[[1]]$code)
      value <- as.numeric(data[[item]])
      reversed <- sum(model$lambda[item, ]) < 0
      if (reversed) max(codes) + min(codes) - value else value
    })
    data[[paste0(name, "_score")]] <- Reduce(`+`, scores)
  }
  data
}

#' Plot imaginary trajectories
#'
#' Mean scale score at each event by group, with every person's line faintly
#' behind it. Needs ggplot2.
#'
#' @param data Output of [synthesize()].
#' @param scale A measurement block name. Defaults to the first one.
#' @return A ggplot.
#' @export
plot_trajectories <- function(data, scale = NULL) {
  rlang::check_installed("ggplot2")
  spec <- attr(data, "spec")
  project <- spec_project(spec)
  scale <- scale %||% names(spec$measurement)[1]
  group_field <- spec$design$groups$field

  scored <- score_scales(data)
  score_col <- paste0(scale, "_score")
  events <- study_events(spec, project)
  groups <- if (is.null(group_field)) {
    tibble::tibble(record_id = unique(scored[[1]]), group = "everyone")
  } else {
    scored |>
      dplyr::filter(!is.na(.data[[group_field]])) |>
      dplyr::distinct(record_id = .data[[names(scored)[1]]], group = .data[[group_field]])
    labels <- field_info(project, group_field)$choices[[1]]
    groups$group <- labels$label[match(groups$group, labels$code)]
  }

  if ("redcap_repeat_instance" %in% names(scored)) {
    scored <- dplyr::filter(scored, is.na(redcap_repeat_instance))
  }
  plot_data <- scored |>
    dplyr::rename(record_id = 1) |>
    dplyr::filter(!is.na(.data[[score_col]])) |>
    dplyr::inner_join(groups, by = "record_id") |>
    dplyr::left_join(dplyr::select(events, redcap_event_name = unique_event_name, day_offset),
                     by = "redcap_event_name") |>
    dplyr::mutate(score = .data[[score_col]])

  ggplot2::ggplot(plot_data, ggplot2::aes(day_offset, score, colour = group)) +
    ggplot2::geom_line(ggplot2::aes(group = record_id), alpha = 0.12) +
    ggplot2::stat_summary(fun = mean, geom = "line", linewidth = 1.3) +
    ggplot2::stat_summary(fun = mean, geom = "point", size = 2.5) +
    ggplot2::labs(x = "Study day", y = paste(scale, "score"), colour = group_field %||% "",
                  title = paste("Imaginary", scale, "trajectories"),
                  subtitle = "Faint lines are individual (made-up) people") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}
