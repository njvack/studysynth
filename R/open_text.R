# Open-text answers.
#
# By default, notes fields get canned phrases whose tone follows the
# person's latent state (a gloomier imaginary person writes gloomier
# feedback). With `provider:` set in `open_text:`, answers come from a
# language model via ellmer instead, prompted with the person's simulated
# values, and cached so reruns give the same text.

add_open_text <- function(records, spec, project, occasions) {
  notes <- project$fields |>
    dplyr::filter(field_type == "notes", field_name %in% names(records))
  first_latent <- names(spec$latents)[1]

  for (field in notes$field_name) {
    def <- spec$open_text[[field]] %||% list()
    has_form <- !is.na(records[[complete_field(field_info(project, field)$form_name)]])
    answered <- has_form & stats::runif(nrow(records)) >= (def$skip_prob %||% 0.4)
    # Abandoned forms already blanked this field; leave it that way.
    answered <- answered & !(records[[complete_field(field_info(project, field)$form_name)]] %in% "0")
    rows <- which(answered)
    if (length(rows) == 0) next

    context <- occasions[match(records$occasion_id[rows], occasions$occasion_id), ]
    mood <- if (is.null(first_latent)) rep(0, length(rows)) else context[[first_latent]]

    records[[field]][rows] <- if (is.null(def$provider)) {
      canned_text(field, mood)
    } else {
      llm_text(def, field_info(project, field), context)
    }
  }
  records
}

canned_text <- function(field, mood) {
  tone <- dplyr::case_when(mood > 0.7 ~ "low", mood < -0.7 ~ "high", .default = "middle")
  bank <- if (stringr::str_detect(field, "doing|activity|where")) activity_phrases else feedback_phrases
  purrr::map_chr(tone, \(t) sample(bank[[t]], 1))
}

feedback_phrases <- list(
  high = c(
    "Honestly this was really nice. I looked forward to it.",
    "The breathing exercises helped more than I expected.",
    "Thanks for running this, I learned a lot about myself.",
    "Great experience overall, the staff were friendly."
  ),
  middle = c(
    "It was fine. Some of the questions were repetitive.",
    "The surveys took longer than I thought they would.",
    "Mostly good. The phone reminders were a bit much.",
    "No complaints.",
    "It was okay. Hard to find time some weeks."
  ),
  low = c(
    "Hard to keep up with everything this month.",
    "I'm not sure it helped, but I tried.",
    "Too many surveys when I was already feeling bad.",
    "Some weeks I didn't have the energy to answer."
  )
)

activity_phrases <- list(
  high = c("Walking the dog", "Coffee with a friend", "Cooking dinner",
           "Reading in the park", "Playing guitar", "At the gym"),
  middle = c("Working", "Commuting", "Watching TV", "Doing laundry",
             "Answering email", "In a meeting", "Grocery shopping", "Writing my thesis"),
  low = c("Lying in bed", "Trying to sleep", "Scrolling my phone", "Nothing really",
          "Sitting at my desk not getting anything done")
)

# Language model answers. Each prompt is filled in from the person's
# simulated values with {glue}-style placeholders, e.g. {age} or {dep}.
llm_text <- function(def, info, context) {
  rlang::check_installed("ellmer", "to write open text with a language model")
  cache_dir <- def$cache %||% file.path(tools::R_user_dir("studysynth", "cache"), "open_text")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  template <- def$prompt %||% paste(
    "You are an imaginary research participant. In one or two casual sentences,",
    "answer this survey question: \"{field_label}\". Do not mention numbers."
  )
  context$field_label <- info$field_label
  prompts <- purrr::map_chr(seq_len(nrow(context)), \(i) {
    as.character(glue_data_row(template, context[i, ]))
  })

  purrr::map_chr(prompts, \(prompt) {
    key <- digest::digest(list(prompt, def$provider, def$model))
    cache_file <- file.path(cache_dir, paste0(key, ".rds"))
    if (file.exists(cache_file)) return(readRDS(cache_file))
    chat <- ellmer::chat(paste0(def$provider, "/", def$model %||% ""))
    answer <- as.character(chat$chat(prompt, echo = "none"))
    saveRDS(answer, cache_file)
    answer
  })
}

# Replaces {name} with values from a one-row data frame.
glue_data_row <- function(template, row) {
  names <- stringr::str_match_all(template, "\\{([A-Za-z0-9_.]+)\\}")[[1]][, 2]
  for (name in unique(names)) {
    value <- row[[name]]
    if (is.numeric(value)) value <- round(value, 2)
    template <- stringr::str_replace_all(template, stringr::fixed(paste0("{", name, "}")), as.character(value %||% ""))
  }
  template
}
