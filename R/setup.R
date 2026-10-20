#' Start a studysynth project
#'
#' Makes a folder with everything you need: your REDCap project file, a
#' drafted `spec.yaml`, a `sources/` folder for scale papers, a guide for an
#' LLM helping you draft (`CLAUDE.md`), and checks that run automatically:
#' when R starts in the folder (`.Rprofile`), before git commits
#' (`.githooks/pre-commit`), and when a Claude Code session starts or edits a
#' file (`.claude/settings.json`). It writes only inside `path`.
#'
#' @param path Folder to create.
#' @param xml Path to your REDCap metadata-only project XML.
#' @param ... Or, instead of `xml`: `dictionary`, `events`, `mappings`,
#'   `repeating` CSV paths, as in [read_redcap_project()].
#' @return The path, invisibly.
#' @export
#' @examples
#' path <- file.path(tempdir(), "calm-minds-fake")
#' use_studysynth(path, xml = example_path("CalmMinds_metadata.xml"))
#' list.files(path, all.files = TRUE, recursive = TRUE)
use_studysynth <- function(path, xml = NULL, ...) {
  csvs <- list(...)
  dir.create(file.path(path, "redcap"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "sources"), showWarnings = FALSE)

  # Copy the REDCap files in, after making sure they read cleanly (and that
  # the XML has no records).
  project <- read_redcap_project(xml = xml, dictionary = csvs$dictionary, events = csvs$events,
                                 mappings = csvs$mappings, repeating = csvs$repeating)
  files <- purrr::compact(c(list(xml = xml), csvs))
  redcap <- purrr::map(files, \(file) {
    file.copy(file, file.path(path, "redcap", basename(file)))
    file.path("redcap", basename(file))
  })

  spec_path <- file.path(path, "spec.yaml")
  if (file.exists(spec_path)) {
    cli::cli_alert_warning("{.file spec.yaml} already exists; leaving it alone.")
  } else {
    write_spec(draft_spec(project, redcap = redcap), spec_path)
  }

  template <- \(file) system.file("templates", file, package = "studysynth")
  file.copy(template("CLAUDE.md"), file.path(path, "CLAUDE.md"))
  write_hooks(path)

  cli::cli_alert_success("Made {.path {path}}.")
  cli::cli_bullets(c(
    ">" = "Put papers and manuals for your measures in {.path sources/}.",
    ">" = "Ask an LLM to fill in {.file spec.yaml} from them ({.file CLAUDE.md} tells it how), or edit it yourself with {.run studysynth::edit_spec()}.",
    ">" = "Then: {.code x <- synthesize(\"spec.yaml\"); study_flow(x)}",
    "i" = "Using git? Run {.code git config core.hooksPath .githooks} to check the spec before each commit."
  ))
  invisible(path)
}

write_hooks <- function(path) {
  writeLines(c(
    "# studysynth: check the spec when R starts here.",
    "if (interactive() && requireNamespace(\"studysynth\", quietly = TRUE) && file.exists(\"spec.yaml\")) {",
    "  local({",
    "    issues <- studysynth::check_spec(\"spec.yaml\", smoke = FALSE)",
    "    n_err <- sum(issues$level == \"error\")",
    "    if (n_err == 0) message(\"studysynth: spec.yaml checks out.\")",
    "    else message(\"studysynth: spec.yaml has \", n_err, \" error(s). Run studysynth::check_spec(\\\"spec.yaml\\\").\")",
    "  })",
    "}"
  ), file.path(path, ".Rprofile"))

  dir.create(file.path(path, ".githooks"), showWarnings = FALSE)
  hook <- file.path(path, ".githooks", "pre-commit")
  writeLines(c(
    "#!/bin/sh",
    "# studysynth: refuse to commit a spec with errors.",
    "Rscript -e 'quit(status = studysynth::check_spec_cli(\"spec.yaml\"))' || {",
    "  echo \"spec.yaml has errors; fix them or commit with --no-verify.\"",
    "  exit 1",
    "}"
  ), hook)
  Sys.chmod(hook, "755")

  # Claude Code: report status at session start; after each edit, send any
  # errors back to Claude (exit code 2 does that).
  dir.create(file.path(path, ".claude"), showWarnings = FALSE)
  check <- "Rscript -e 'quit(status = studysynth::check_spec_cli(\"spec.yaml\", exit_code = %d))'"
  settings <- list(hooks = list(
    SessionStart = list(list(hooks = list(list(type = "command", command = sprintf(check, 0L))))),
    PostToolUse = list(list(
      matcher = "Edit|Write",
      hooks = list(list(type = "command", command = paste(sprintf(check, 2L), "1>&2")))
    ))
  ))
  jsonlite::write_json(settings, file.path(path, ".claude", "settings.json"),
                       auto_unbox = TRUE, pretty = TRUE)
}
