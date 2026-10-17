example_spec <- function() read_spec(example_path("calm_minds.yaml"))
n_people <- 15

unclass_for_compare <- function(x) tibble::as_tibble(unclass(x)[names(x)])

# One shared run keeps the suite fast.
fake <- synthesize(example_spec(), n = n_people, seed = 1, quiet = TRUE)
status <- attr(fake, "status")

test_that("the same seed gives the same data", {
  again <- synthesize(example_spec(), n = n_people, seed = 1, quiet = TRUE)

  expect_identical(unclass_for_compare(fake), unclass_for_compare(again))
})

test_that("output has REDCap's columns", {
  expected <- c(
    "record_id", "redcap_event_name", "redcap_repeat_instrument",
    "redcap_repeat_instance", "phq9_complete", "pss_complete",
    "ema_mood_complete", "race___1", "race___6"
  )

  expect_true(all(expected %in% names(fake)))
  expect_true(all(paste0("race___", 1:6) %in% names(fake)))
})

test_that("exactly n people are enrolled", {
  enrolled <- status$status %in% c("completed", "withdrew")

  expect_equal(sum(enrolled), n_people)
})

test_that("people who aren't enrolled only have screening rows", {
  not_enrolled <- status$record_id[!status$status %in% c("completed", "withdrew")]
  rows <- fake[fake$record_id %in% not_enrolled, ]

  expect_gt(length(not_enrolled), 0)
  expect_true(all(rows$redcap_event_name == "screening_arm_1"))
})

test_that("ema rows count up from 1 per person and have timestamps", {
  ema <- fake[fake$redcap_repeat_instrument %in% "ema_mood", ]
  instances <- split(ema$redcap_repeat_instance, ema$record_id)

  expect_gt(nrow(ema), 0)
  expect_true(all(purrr::map_lgl(instances, \(i) identical(as.integer(i), seq_along(i)))))
  # Abandoned prompts (marked incomplete) are blank, including the timestamp.
  finished <- ema[ema$ema_mood_complete == "2", ]
  expect_false(anyNA(finished$ema_ts))
})

test_that("phq9_total is the sum of the answered items", {
  items <- paste0("phq9_", 1:9)
  rows <- fake[!is.na(fake$phq9_total), ]
  answers <- as.matrix(rows[items])
  mode(answers) <- "numeric"

  expect_gt(nrow(rows), 0)
  expect_equal(as.numeric(rows$phq9_total), rowSums(answers, na.rm = TRUE))
})

test_that("phq9_10 is blank when phq9_total is 0", {
  # Make zeros common: high thresholds, and no screening to filter them out.
  spec <- example_spec()
  spec$screening <- NULL
  spec$measurement$phq9$thresholds <- list(default = c(5, 6, 7))
  zeros <- synthesize(spec, n = 5, quiet = TRUE)
  zeros <- zeros[zeros$phq9_total %in% "0", ]

  expect_gt(nrow(zeros), 0)
  expect_true(all(is.na(zeros$phq9_10)))
})

test_that("withdrawn people have no rows from their withdrawal event on", {
  spec <- example_spec()
  events <- study_events(spec, spec_project(spec))$unique_event_name
  withdrawn <- status[status$status == "withdrew", ]
  later <- purrr::map2(withdrawn$record_id, withdrawn$withdrawn_at, \(id, event) {
    fake[fake$record_id == id & match(fake$redcap_event_name, events) >= match(event, events), ]
  })

  expect_gt(nrow(withdrawn), 0)
  expect_equal(sum(purrr::map_int(later, nrow)), 0)
})

test_that("write_redcap_csv writes the data and a manifest", {
  path <- tempfile(fileext = ".csv")
  write_redcap_csv(fake, path)
  manifest <- yaml::read_yaml(paste0(path, ".manifest.yaml"))

  expect_equal(nrow(readr::read_csv(path, show_col_types = FALSE)), nrow(fake))
  expect_equal(manifest$n_rows, nrow(fake))
})
