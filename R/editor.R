#' Edit a spec with a live preview
#'
#' Opens a Shiny app with the spec's YAML on the left and, on the right, the
#' checker's verdict and a fresh batch of imaginary participants that updates
#' as you type. Tweak a loading or an effect size and watch the trajectories
#' move. Save writes your text exactly as typed (comments included), even if
#' it has errors, so you never lose work.
#'
#' Needs shiny, bslib, and ggplot2.
#'
#' @param path Path to the spec.
#' @param preview_n How many people to simulate for the preview.
#' @export
#' @examples
#' \dontrun{
#' edit_spec("spec.yaml")
#' }
edit_spec <- function(path = "spec.yaml", preview_n = 80) {
  rlang::check_installed(c("shiny", "bslib", "ggplot2"))
  shiny::runApp(editor_app(path, preview_n))
}

editor_app <- function(path, preview_n = 80) {
  dir <- dirname(normalizePath(path))
  initial <- paste(readLines(path, warn = FALSE), collapse = "\n")

  ui <- bslib::page_sidebar(
    title = paste("studysynth:", basename(path)),
    sidebar = bslib::sidebar(
      width = "45%",
      shiny::tags$style("#yaml { font-family: monospace; font-size: 12px; white-space: pre; overflow-x: auto; }"),
      shiny::textAreaInput("yaml", NULL, initial, width = "100%", rows = 38, resize = "vertical"),
      shiny::div(
        shiny::actionButton("save", "Save", class = "btn-primary"),
        shiny::actionButton("reroll", "New people"),
        shiny::textOutput("status", inline = TRUE)
      )
    ),
    bslib::navset_card_tab(
      bslib::nav_panel("Checker", shiny::uiOutput("issues")),
      bslib::nav_panel(
        "Trajectories",
        shiny::selectInput("scale", "Scale", choices = NULL),
        shiny::plotOutput("trajectories", height = "450px")
      ),
      bslib::nav_panel(
        "Items",
        shiny::selectInput("event", "Event", choices = NULL),
        shiny::plotOutput("items", height = "450px")
      ),
      bslib::nav_panel("Flow", shiny::verbatimTextOutput("flow")),
      bslib::nav_panel("Data", shiny::tableOutput("data"))
    )
  )

  server <- function(input, output, session) {
    text <- shiny::debounce(shiny::reactive(input$yaml), 1000)

    spec <- shiny::reactive({
      tryCatch(parse_spec(text(), dir = dir), error = \(e) e)
    })

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
      tryCatch(
        synthesize(s, n = min(s$design$n_subjects %||% preview_n, preview_n),
                   seed = sample.int(1e6, 1), check = FALSE, quiet = TRUE),
        error = \(e) e
      )
    })

    shiny::observe({
      s <- spec()
      if (inherits(s, "error")) return()
      scales <- names(s$measurement)
      shiny::updateSelectInput(session, "scale", choices = scales,
                               selected = if (isTRUE(input$scale %in% scales)) input$scale else scales[1])
      events <- tryCatch(spec_project(s)$events$unique_event_name, error = \(e) character())
      shiny::updateSelectInput(session, "event", choices = events,
                               selected = if (isTRUE(input$event %in% events)) input$event else events[1])
    })

    output$issues <- shiny::renderUI({
      x <- issues()
      if (nrow(x) == 0) return(shiny::p("Spec looks good. Go make some imaginary people."))
      colors <- c(error = "#b91c1c", warning = "#b45309", info = "#1d4ed8")
      shiny::tags$ul(
        class = "list-unstyled",
        purrr::pmap(x, \(level, path, message, hint) {
          shiny::tags$li(
            style = "margin-bottom: 0.6em",
            shiny::tags$strong(style = paste0("color:", colors[[level]]), level), " ",
            shiny::tags$code(path), " ", message,
            if (!is.na(hint)) shiny::div(style = "color: #6b7280; margin-left: 1em", hint)
          )
        })
      )
    })

    preview_or_message <- function() {
      x <- preview()
      shiny::validate(
        shiny::need(!is.null(x), "Fix the errors in the Checker tab to see a preview."),
        shiny::need(!inherits(x, "error"), if (inherits(x, "error")) conditionMessage(x))
      )
      x
    }

    output$trajectories <- shiny::renderPlot({
      shiny::req(input$scale)
      plot_trajectories(preview_or_message(), input$scale)
    })
    output$items <- shiny::renderPlot({
      shiny::req(input$scale, input$event)
      plot_items(preview_or_message(), input$scale, input$event)
    })
    output$flow <- shiny::renderPrint(study_flow(preview_or_message()))
    output$data <- shiny::renderTable(utils::head(unclass_data(preview_or_message()), 50))

    output$status <- shiny::renderText({
      n_err <- sum(issues()$level == "error")
      if (n_err == 0) "  No errors" else paste0("  ", n_err, " error(s)")
    })

    shiny::observeEvent(input$save, {
      writeLines(input$yaml, path)
      has_errors <- any(issues()$level == "error")
      shiny::showNotification(
        if (has_errors) "Saved, but the spec has errors." else "Saved.",
        type = if (has_errors) "warning" else "message"
      )
    })
  }

  shiny::shinyApp(ui, server)
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
