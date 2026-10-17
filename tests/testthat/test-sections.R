lines <- readLines(example_path("calm_minds.yaml"))
sections <- spec_sections(lines)

test_that("each measure and latent is its own section", {
  expect_true(all(c("measurement.phq9", "latents.dep", "screening", "ema.ema_mood") %in% sections$id))
})

test_that("putting a section back unchanged changes nothing", {
  for (i in seq_len(nrow(sections))) {
    expect_identical(replace_section(lines, sections[i, ], section_text(lines, sections[i, ])), lines)
  }
})

test_that("editing a section keeps comments elsewhere", {
  pss <- sections[sections$id == "measurement.pss", ]
  edited <- replace_section(lines, pss, sub("0.70*pss_1", "0.75*pss_1", section_text(lines, pss), fixed = TRUE))
  spec <- parse_spec(paste(edited, collapse = "\n"))

  expect_match(spec$measurement$pss$model, "0.75*pss_1", fixed = TRUE)
  expect_equal(sum(grepl("^ *#", edited)), sum(grepl("^ *#", lines)))
})

test_that("section_of maps issue paths to sections", {
  expect_equal(section_of(c("measurement.phq9.thresholds", "screening.event", "(smoke test)")),
               c("measurement.phq9", "screening", "(smoke test)"))
})
