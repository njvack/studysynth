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
#' Adds a score column for each scale: `<block>_score` for a one-factor
#' block, or one `<factor>_score` per subscale for a block with several
#' factors (PANAS gives `pa_score` and `na_score`). Each item counts toward
#' the factor it loads on most strongly, reverse-scored if that loading is
#' negative. Rows missing any of a score's items get `NA`.
#'
#' @param data Output of [synthesize()].
#' @return `data` with score columns added.
#' @export
score_scales <- function(data) {
  spec <- attr(data, "spec")
  project <- spec_project(spec)
  for (name in names(spec$measurement)) {
    for (sub in subscales(name, spec$measurement[[name]])) {
      scores <- purrr::map2(sub$items, sub$sign, \(item, sign) {
        codes <- as.numeric(field_info(project, item)$choices[[1]]$code)
        value <- as.numeric(data[[item]])
        if (sign < 0) max(codes) + min(codes) - value else value
      })
      data[[paste0(sub$name, "_score")]] <- Reduce(`+`, scores)
    }
  }
  data
}

#' Plot imaginary trajectories
#'
#' Mean scale score at each event by group, with up to 100 people's lines
#' faintly behind. A scale with subscales gets one panel per subscale.
#' Needs ggplot2.
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
  score_names <- purrr::map_chr(subscales(scale, spec$measurement[[scale]]), "name")

  scored <- score_scales(data)
  if ("redcap_repeat_instance" %in% names(scored)) {
    scored <- dplyr::filter(scored, is.na(redcap_repeat_instance))
  }
  events <- study_events(spec, project)
  plot_data <- scored |>
    dplyr::rename(record_id = 1) |>
    dplyr::select(record_id, redcap_event_name, dplyr::all_of(paste0(score_names, "_score"))) |>
    tidyr::pivot_longer(-c(record_id, redcap_event_name), names_to = "subscale", values_to = "score") |>
    dplyr::filter(!is.na(score)) |>
    dplyr::mutate(subscale = sub("_score$", "", subscale)) |>
    dplyr::inner_join(participant_groups(data, spec, project), by = "record_id") |>
    dplyr::left_join(dplyr::select(events, redcap_event_name = unique_event_name, day_offset),
                     by = "redcap_event_name")

  # Drawing every person's line gets slow and muddy past a hundred or so.
  ids <- unique(plot_data$record_id)
  shown <- sample(ids, min(100, length(ids)))

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(day_offset, score, colour = group)) +
    ggplot2::geom_line(data = dplyr::filter(plot_data, record_id %in% shown),
                       ggplot2::aes(group = interaction(record_id, subscale)), alpha = 0.12) +
    ggplot2::stat_summary(fun = mean, geom = "line", linewidth = 1.3) +
    ggplot2::stat_summary(fun = mean, geom = "point", size = 2.5) +
    ggplot2::labs(x = "Study day", y = "Score", colour = group_field %||% "",
                  title = paste("Imaginary", scale, "trajectories"),
                  subtitle = "Faint lines are individual (made-up) people, up to 100") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  if (length(score_names) > 1) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(subscale), scales = "free_y")
  }
  plot
}

#' Plot item response distributions
#'
#' One bar chart per item: how often each response was chosen at an event.
#'
#' @param data Output of [synthesize()].
#' @param scale A measurement block name.
#' @param event A unique event name. Defaults to all events.
#' @return A ggplot.
#' @export
plot_items <- function(data, scale = NULL, event = NULL) {
  rlang::check_installed("ggplot2")
  spec <- attr(data, "spec")
  scale <- scale %||% names(spec$measurement)[1]
  items <- parse_measurement(spec$measurement[[scale]])$items
  if (!is.null(event)) data <- data[data$redcap_event_name %in% event, ]

  long <- unclass_data(data) |>
    dplyr::select(dplyr::all_of(items)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "item", values_to = "response") |>
    dplyr::filter(!is.na(response)) |>
    dplyr::mutate(item = factor(item, levels = items))

  ggplot2::ggplot(long, ggplot2::aes(response)) +
    ggplot2::geom_bar(fill = "#4f6d7a") +
    ggplot2::facet_wrap(ggplot2::vars(item)) +
    ggplot2::labs(x = "Response code", y = "Count",
                  title = paste(scale, "responses", if (!is.null(event)) paste("at", event) else "")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

#' Plot a latent variable's true trajectories
#'
#' The simulated latent values (not item responses), averaged by study day
#' and group, with some individual people faintly behind.
#'
#' @param data Output of [synthesize()].
#' @param latent A latent name from the spec's `latents:` block.
#' @param max_people How many individual lines to draw.
#' @return A ggplot.
#' @export
plot_latent <- function(data, latent = NULL, max_people = 60) {
  rlang::check_installed("ggplot2")
  spec <- attr(data, "spec")
  latent <- latent %||% names(spec$latents)[1]
  group_field <- spec$design$groups$field

  scores <- true_scores(data) |>
    dplyr::select(-dplyr::any_of(group_field)) |>
    dplyr::inner_join(participant_groups(data, spec, spec_project(spec)), by = "record_id") |>
    dplyr::mutate(day = floor(time * (spec$design$time$unit_days %||% 1)), value = .data[[latent]]) |>
    dplyr::summarise(value = mean(value), .by = c(record_id, group, day))
  shown <- sample(unique(scores$record_id), min(max_people, length(unique(scores$record_id))))

  ggplot2::ggplot(scores, ggplot2::aes(day, value, colour = group)) +
    ggplot2::geom_line(data = dplyr::filter(scores, record_id %in% shown),
                       ggplot2::aes(group = record_id), alpha = 0.15) +
    ggplot2::stat_summary(fun = mean, geom = "line", linewidth = 1.3) +
    ggplot2::labs(x = "Study day", y = latent, colour = group_field %||% "",
                  title = paste("True", latent, "(latent scale)"),
                  subtitle = "Daily averages; faint lines are individual people") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

#' Plot retention across events
#'
#' Share of enrolled participants with any data at each event, by group.
#'
#' @param data Output of [synthesize()].
#' @return A ggplot.
#' @export
plot_retention <- function(data) {
  rlang::check_installed("ggplot2")
  spec <- attr(data, "spec")
  project <- spec_project(spec)
  status <- attr(data, "status")
  enrolled <- status$record_id[status$status %in% enrolled_statuses]
  group_field <- spec$design$groups$field
  ids <- data[[1]]

  groups <- participant_groups(data, spec, project)
  events <- study_events(spec, project)
  present <- tibble::tibble(record_id = ids, unique_event_name = data$redcap_event_name) |>
    dplyr::filter(record_id %in% enrolled) |>
    dplyr::distinct()

  retention <- tidyr::expand_grid(groups, events) |>
    dplyr::left_join(dplyr::mutate(present, here = TRUE), by = c("record_id", "unique_event_name")) |>
    dplyr::summarise(retained = mean(!is.na(here)), .by = c(group, event_name, day_offset))

  ggplot2::ggplot(retention, ggplot2::aes(day_offset, retained, colour = group)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_y_continuous(labels = \(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
    ggplot2::labs(x = "Study day", y = "Enrolled people with data", colour = group_field %||% "",
                  title = "Retention by event") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

# Each enrolled person's group, labeled the way the REDCap field labels it.
participant_groups <- function(data, spec, project) {
  group_field <- spec$design$groups$field
  status <- attr(data, "status")
  enrolled <- status$record_id[status$status %in% enrolled_statuses]
  if (is.null(group_field)) {
    return(tibble::tibble(record_id = enrolled, group = "everyone"))
  }
  labels <- field_info(project, group_field)$choices[[1]]
  data |>
    dplyr::filter(!is.na(.data[[group_field]])) |>
    dplyr::distinct(record_id = .data[[names(data)[1]]], group = .data[[group_field]]) |>
    dplyr::mutate(group = labels$label[match(group, labels$code)])
}
