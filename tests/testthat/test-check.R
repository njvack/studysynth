example_spec <- function() read_spec(example_path("calm_minds.yaml"))

# Returns the issues of a given level, from a spec without the smoke test.
issues_of <- function(spec, level = "error") {
  out <- check_spec(spec, smoke = FALSE)
  out[out$level == level, ]
}

expect_issue_at <- function(spec, path, level = "error") {
  expect_true(path %in% issues_of(spec, level)$path)
}

test_that("the example spec has no errors", {
  expect_equal(nrow(issues_of(example_spec())), 0)
})

test_that("the smoke test passes on the example", {
  out <- check_spec(example_spec())

  expect_false(any(out$level == "error"))
})

test_that("a misspelled item gets a hint", {
  spec <- example_spec()
  spec$measurement$phq9$model <- sub("phq9_2", "phq9_two", spec$measurement$phq9$model)
  errors <- issues_of(spec)

  expect_true("measurement.phq9.model" %in% errors$path)
  expect_true(any(grepl("Did you mean", errors$hint)))
})

test_that("the wrong number of thresholds is an error", {
  spec <- example_spec()
  spec$measurement$phq9$thresholds$default <- c(-0.3, 0.7)

  expect_issue_at(spec, "measurement.phq9.thresholds")
})

test_that("a loading without a number is an error", {
  spec <- example_spec()
  spec$measurement$phq9$model <- "dep =~ phq9_1 + 0.8*phq9_2 + 0.7*phq9_3"

  expect_issue_at(spec, "measurement.phq9.model")
})

test_that("a fixef name that doesn't match the formula is an error", {
  spec <- example_spec()
  names(spec$latents$dep$fixef)[3] <- "rand_groupmindfulness"
  errors <- issues_of(spec)

  expect_true("latents.dep.fixef.rand_groupmindfulness" %in% errors$path)
  expect_true("latents.dep.fixef" %in% errors$path)
})

test_that("a missing fixef is an error", {
  spec <- example_spec()
  spec$latents$dep$fixef$time <- NULL

  expect_issue_at(spec, "latents.dep.fixef")
})

test_that("a ranef group missing from the formula is a warning", {
  spec <- example_spec()
  spec$latents$dep$ranef$site <- list(sd = 0.5)

  expect_issue_at(spec, "latents.dep.ranef.site", level = "warning")
  expect_equal(nrow(issues_of(spec)), 0)
})

test_that("a latent used in measurement needs a latents block", {
  spec <- example_spec()
  spec$latents <- NULL

  expect_issue_at(spec, "measurement.phq9")
})

test_that("a bad screening event is an error", {
  spec <- example_spec()
  spec$screening$event <- "nowhere_arm_1"

  expect_issue_at(spec, "screening.event")
})

test_that("a probability above 1 is an error", {
  spec <- example_spec()
  spec$missingness$missed_visit <- 1.5

  expect_issue_at(spec, "missingness.missed_visit")
})

test_that("an ema form that isn't repeating is an error", {
  spec <- example_spec()
  spec$ema$ema_mood$event <- "baseline_arm_1"
  spec$ema$ema_mood$form <- "phq9"

  expect_issue_at(spec, "ema.ema_mood.form")
})

test_that("synthesize() refuses a spec with errors", {
  spec <- example_spec()
  spec$screening$event <- "nowhere_arm_1"

  expect_error(
    suppressMessages(synthesize(spec, n = 5, quiet = TRUE)),
    "errors"
  )
})
