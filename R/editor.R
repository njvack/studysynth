#' Edit a spec with a live preview
#'
#' Opens a Shiny app for editing a spec one block at a time. The left side
#' lists every block (each measure, each latent, screening, missingness...)
#' with its error and warning counts, and a search box for when there are
#' dozens. Pick one and edit its YAML; the right side shows the checker's
#' issues for that block and a preview aimed at it: item response bars for a
#' measure, true trajectories for a latent, compliance for experience
#' sampling, retention for missingness.
#'
#' Blocks are edited as text, so comments survive. Save writes the file even
#' if it has errors, so you never lose work. The preview simulates a smaller
#' sample than the full study to stay quick; change its size at the top.
#'
#' Needs shiny, bslib, and ggplot2 (and DT for the data table).
#'
#' @param path Path to the spec.
#' @param preview_n How many people to enroll for the preview.
#' @export
#' @examples
#' \dontrun{
#' edit_spec("spec.yaml")
#' }
edit_spec <- function(path = "spec.yaml", preview_n = 150) {
  rlang::check_installed(c("shiny", "bslib", "ggplot2"))
  shiny::runApp(editor_app(path, preview_n))
}

editor_app <- function(path, preview_n = 150) {
  dir <- dirname(normalizePath(path))
  whole_file <- "(whole file)"

  ui <- bslib::page_sidebar(
    title = shiny::div(
      paste("studysynth:", basename(path)),
      shiny::span(style = "margin-left: 2em; font-size: 0.9rem", shiny::textOutput("status", inline = TRUE))
    ),
    sidebar = bslib::sidebar(
      width = 300,
      shiny::textInput("find", NULL, placeholder = "Find a block..."),
      shiny::div(
        style = "max-height: calc(100vh - 220px); overflow-y: auto",
        shiny::radioButtons("section", NULL, choices = whole_file)
      )
    ),
    shiny::tags$style(shiny::HTML("
      #block { font-family: monospace; font-size: 12px; white-space: pre; overflow-x: auto; }
      .issue { margin-bottom: 0.5em; } .hint { color: #6b7280; margin-left: 1em; }
      .shiny-options-group .badge { margin-left: 0.3em; font-size: 0.7em; }
      .block-kind { color: #6b7280; font-size: 0.8em; }
    ")),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          shiny::textOutput("section_title", inline = TRUE),
          shiny::div(
            shiny::actionButton("save", "Save", class = "btn-primary btn-sm"),
            shiny::actionButton("reroll", "New people", class = "btn-sm")
          )
        ),
        shiny::textAreaInput("block", NULL, "", width = "100%", rows = 22, resize = "vertical"),
        shiny::uiOutput("block_issues")
      ),
      bslib::navset_card_tab(
        bslib::nav_panel("This block", shiny::uiOutput("focus")),
        bslib::nav_panel("Study flow", shiny::verbatimTextOutput("flow"), shiny::plotOutput("retention", height = "320px")),
        bslib::nav_panel("All issues", shiny::uiOutput("all_issues")),
        bslib::nav_panel("Data", if (requireNamespace("DT", quietly = TRUE)) DT::DTOutput("data") else shiny::tableOutput("data")),
        bslib::nav_panel(
          "Settings",
          shiny::numericInput("preview_n", "People to enroll in the preview", preview_n, min = 10, step = 10),
          shiny::p("The full study size is design.n_subjects. Previews use fewer people so they stay quick.")
        )
      )
    )
  )

  server <- function(input, output, session) {
    lines <- shiny::reactiveVal(readLines(path, warn = FALSE))
    loaded <- shiny::reactiveVal(NULL) # which section the text box holds

    sections <- shiny::reactive(spec_sections(lines()))

    current_section <- function() {
      id <- input$section
      if (is.null(id) || id == whole_file) return(NULL)
      s <- sections()
      s[s$id == id, ]
    }

    # Show the chosen block's text.
    shiny::observeEvent(list(input$section, sections()), {
      if (identical(loaded(), input$section)) return()
      section <- current_section()
      text <- if (is.null(section) || nrow(section) == 0) {
        paste(lines(), collapse = "\n")
      } else {
        section_text(lines(), section)
      }
      loaded(input$section)
      shiny::updateTextAreaInput(session, "block", value = text)
    })

    # Edits to the block go straight back into the whole text.
    shiny::observeEvent(input$block, {
      shiny::req(identical(loaded(), input$section))
      section <- current_section()
      if (is.null(section)) {
        new <- strsplit(input$block, "\n", fixed = TRUE)[[1]]
        loaded(NULL) # sections may have moved; reload after
      } else {
        shiny::req(nrow(section) == 1, input$block != section_text(lines(), section))
        new <- replace_section(lines(), section, input$block)
      }
      if (!identical(new, lines())) lines(new)
    }, ignoreInit = TRUE)

    text <- shiny::debounce(shiny::reactive(paste(lines(), collapse = "\n")), 800)

    spec <- shiny::reactive(tryCatch(parse_spec(text(), dir = dir), error = \(e) e))

    issues <- shiny::reactive({
      s <- spec()
      if (inherits(s, "error")) {
        return(as_issues(issue("error", "(yaml)", paste("The YAML doesn't parse:", conditionMessage(s)))))
      }
      check_spec(s, smoke = FALSE)
    })

    preview <- shiny::reactive({
      input$reroll
      if (any(issues()$level == "error")) return(NULL)
      s <- spec()
      n <- min(s$design$n_subjects %||% input$preview_n, input$preview_n)
      tryCatch(
        synthesize(s, n = n, seed = sample.int(1e6, 1), check = FALSE, quiet = TRUE),
        error = \(e) e
      )
    })

    preview_or_message <- function() {
      x <- preview()
      shiny::validate(
        shiny::need(!is.null(x), "Fix the errors (see All issues) to see a preview."),
        shiny::need(!inherits(x, "error"), if (inherits(x, "error")) conditionMessage(x))
      )
      x
    }

    # The block list, with issue counts, filtered by the search box.
    shiny::observe({
      s <- sections()
      x <- issues()
      find <- tolower(input$find %||% "")
      keep <- find == "" | grepl(find, tolower(s$id), fixed = TRUE)
      s <- s[keep, ]
      names <- purrr::pmap(s, \(id, block, name, ...) {
        counts <- issue_counts(x, id)
        shiny::span(
          if (block != name) shiny::span(class = "block-kind", block), name,
          if (counts[["error"]] > 0) shiny::span(class = "badge bg-danger", counts[["error"]]),
          if (counts[["warning"]] > 0) shiny::span(class = "badge bg-warning text-dark", counts[["warning"]])
        )
      })
      selected <- shiny::isolate(input$section) %||% whole_file
      if (!selected %in% c(whole_file, s$id)) selected <- whole_file
      shiny::updateRadioButtons(session, "section",
                                choiceNames = c(list(whole_file), names),
                                choiceValues = c(whole_file, s$id), selected = selected)
    })

    output$section_title <- shiny::renderText(input$section %||% whole_file)

    output$status <- shiny::renderText({
      x <- issues()
      sprintf("%d error(s), %d warning(s)", sum(x$level == "error"), sum(x$level == "warning"))
    })

    output$block_issues <- shiny::renderUI({
      id <- input$section
      x <- issues()
      if (is.null(id) || id == whole_file) {
        x <- x[x$level != "info", ]
      } else {
        x <- x[section_of(x$path) == id, ]
      }
      issue_list(x)
    })
    output$all_issues <- shiny::renderUI(issue_list(issues()))

    output$focus <- shiny::renderUI({
      id <- input$section %||% whole_file
      s <- spec()
      shiny::req(!inherits(s, "error"))
      block <- strsplit(id, ".", fixed = TRUE)[[1]][1]
      name <- sub("^[^.]+\\.", "", id)
      switch(block,
        measurement = {
          form <- s$measurement[[name]]$form
          events <- tryCatch({
            p <- spec_project(s)
            intersect(study_events(s, p)$unique_event_name, p$mappings$unique_event_name[p$mappings$form == form])
          }, error = \(e) character())
          shiny::tagList(
            shiny::selectInput("focus_event", "Event", choices = events, selected = shiny::isolate(input$focus_event)),
            shiny::plotOutput("focus_items", height = "380px"),
            shiny::tableOutput("focus_stats"),
            if (length(events) > 1) shiny::plotOutput("focus_traj", height = "320px")
          )
        },
        latents = shiny::plotOutput("focus_latent", height = "420px"),
        ema = shiny::plotOutput("focus_ema", height = "420px"),
        open_text = shiny::tableOutput("focus_text"),
        covariates = shiny::plotOutput("focus_covariate", height = "320px"),
        shiny::tagList(shiny::verbatimTextOutput("focus_flow"), shiny::plotOutput("focus_retention", height = "320px"))
      )
    })

    focus_name <- shiny::reactive(sub("^[^.]+\\.", "", input$section %||% ""))
    # The name of the chosen block, if it's in `block` (outputs for other
    # kinds of blocks quietly wait).
    focus_in <- function(block) {
      name <- focus_name()
      shiny::req(startsWith(input$section %||% "", paste0(block, ".")), name %in% names(spec()[[block]]))
      name
    }

    output$focus_items <- shiny::renderPlot({
      shiny::req(input$focus_event)
      plot_items(preview_or_message(), focus_in("measurement"), input$focus_event)
    })
    output$focus_stats <- shiny::renderTable(scale_stats(preview_or_message(), focus_in("measurement")), digits = 2)
    output$focus_traj <- shiny::renderPlot(plot_trajectories(preview_or_message(), focus_in("measurement")))
    output$focus_latent <- shiny::renderPlot(plot_latent(preview_or_message(), focus_in("latents")))
    output$focus_ema <- shiny::renderPlot(plot_ema(preview_or_message(), focus_in("ema")))
    output$focus_text <- shiny::renderTable({
      x <- preview_or_message()
      answers <- x[[focus_in("open_text")]]
      answers <- answers[!is.na(answers)]
      tibble::tibble(`Some answers` = utils::head(sample(answers), 12))
    })
    output$focus_covariate <- shiny::renderPlot({
      name <- focus_in("covariates")
      x <- true_people(preview_or_message())
      value <- x[[name]]
      ggplot2::ggplot(tibble::tibble(value = value), ggplot2::aes(value)) +
        (if (is.numeric(value)) ggplot2::geom_histogram(bins = 30, fill = "#4f6d7a") else ggplot2::geom_bar(fill = "#4f6d7a")) +
        ggplot2::labs(x = focus_name(), y = "People", title = paste(focus_name(), "among everyone screened")) +
        ggplot2::theme_minimal()
    })
    output$focus_flow <- output$flow <- shiny::renderPrint(study_flow(preview_or_message()))
    output$focus_retention <- output$retention <- shiny::renderPlot(plot_retention(preview_or_message()))

    output$data <- if (requireNamespace("DT", quietly = TRUE)) {
      DT::renderDT(DT::datatable(unclass_data(preview_or_message()),
                                 options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE))
    } else {
      shiny::renderTable(utils::head(unclass_data(preview_or_message()), 50))
    }

    shiny::observeEvent(input$save, {
      writeLines(lines(), path)
      has_errors <- any(issues()$level == "error")
      shiny::showNotification(
        if (has_errors) "Saved, but the spec has errors." else "Saved.",
        type = if (has_errors) "warning" else "message"
      )
    })
  }

  shiny::shinyApp(ui, server)
}

# Which section an issue path belongs to: "measurement.phq9.thresholds" ->
# "measurement.phq9", "screening.event" -> "screening".
section_of <- function(paths) {
  parts <- strsplit(paths, ".", fixed = TRUE)
  purrr::map_chr(parts, \(p) {
    if (p[1] %in% container_blocks && length(p) > 1) paste(p[1:2], collapse = ".") else p[1]
  })
}

issue_counts <- function(issues, id) {
  here <- issues$level[section_of(issues$path) == id]
  c(error = sum(here == "error"), warning = sum(here == "warning"))
}

issue_list <- function(x) {
  if (nrow(x) == 0) return(shiny::p("No issues here."))
  colors <- c(error = "#b91c1c", warning = "#b45309", info = "#1d4ed8")
  shiny::div(purrr::pmap(x, \(level, path, message, hint) {
    shiny::div(
      class = "issue",
      shiny::tags$strong(style = paste0("color:", colors[[level]]), level), " ",
      shiny::tags$code(path), " ", message,
      if (!is.na(hint)) shiny::div(class = "hint", hint)
    )
  }))
}

# Per-event summary of a scale (or each of its subscales): n, mean and SD
# of the total, alpha.
scale_stats <- function(data, scale) {
  spec <- attr(data, "spec")
  scored <- score_scales(data)
  purrr::map_dfr(subscales(scale, spec$measurement[[scale]]), \(sub) {
    scored |>
      dplyr::mutate(score = .data[[paste0(sub$name, "_score")]]) |>
      dplyr::filter(!is.na(score)) |>
      dplyr::summarise(
        n = dplyr::n(), mean = mean(score), sd = stats::sd(score),
        alpha = cronbach_alpha(dplyr::pick(dplyr::all_of(sub$items)), sub$sign),
        .by = redcap_event_name
      ) |>
      dplyr::mutate(subscale = sub$name, .before = 1)
  }) |>
    dplyr::rename(event = redcap_event_name)
}

# One row per person screened, with covariates and group.
true_people <- function(data) {
  spec <- attr(data, "spec")
  attr(data, "occasions") |>
    dplyr::distinct(record_id, .keep_all = TRUE) |>
    dplyr::select(record_id, dplyr::any_of(c(names(spec$covariates), spec$design$groups$field)))
}

#' Plot experience sampling compliance
#'
#' How many prompts each person answered, and how answering falls off over
#' study days.
#'
#' @param data Output of [synthesize()].
#' @param block An `ema:` block name.
#' @return A ggplot.
#' @export
plot_ema <- function(data, block = NULL) {
  rlang::check_installed("ggplot2")
  spec <- attr(data, "spec")
  block <- block %||% names(spec$ema)[1]
  per_day <- spec$ema[[block]]$prompts_per_day %||% 5
  answered <- attr(data, "occasions") |>
    dplyr::filter(ema_block == block) |>
    dplyr::count(record_id, prompt_day)

  ggplot2::ggplot(answered, ggplot2::aes(factor(prompt_day), n / per_day)) +
    ggplot2::geom_boxplot(fill = "#dbe4e8", outlier.alpha = 0.3) +
    ggplot2::stat_summary(fun = mean, geom = "point", colour = "#b45309", size = 2.5) +
    ggplot2::scale_y_continuous(labels = \(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(x = "Day of the burst", y = "Prompts answered",
                  title = paste(block, "compliance by day"),
                  subtitle = sprintf("Median %d prompts answered per person; orange dots are means",
                                     as.integer(stats::median(dplyr::count(answered, record_id, wt = n)$n)))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}
