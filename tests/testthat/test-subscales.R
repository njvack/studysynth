# PSS-10 as two correlated factors: helplessness and self-efficacy.
two_factor_spec <- function(r = -0.5) {
  spec <- read_spec(example_path("calm_minds.yaml"))
  spec$measurement$pss$model <- paste0(
    "stress =~ 0.70*pss_1 + 0.72*pss_2 + 0.68*pss_3 + 0.66*pss_6 + 0.64*pss_9 + 0.71*pss_10\n",
    "cope =~ 0.55*pss_4 + 0.60*pss_5 + 0.50*pss_7 + 0.62*pss_8\n",
    "stress ~~ ", r, "*cope\n"
  )
  spec$latents$cope <- list(
    formula = "cope ~ 1 + (1 | record_id)",
    fixef = list("(Intercept)" = 0),
    ranef = list(record_id = list(sd = 1)),
    sigma = 0.3
  )
  spec
}

test_that("a two-factor block gives one subscale per factor", {
  subs <- subscales("pss", two_factor_spec()$measurement$pss)

  expect_equal(purrr::map_chr(subs, "name"), c("stress", "cope"))
  expect_equal(subs[[2]]$items, c("pss_4", "pss_5", "pss_7", "pss_8"))
})

test_that("factor correlations correlate people's intercepts", {
  spec <- two_factor_spec(r = -0.6)
  spec$screening <- NULL
  spec$missingness <- NULL
  fake <- synthesize(spec, n = 400, seed = 2, quiet = TRUE)
  intercepts <- true_scores(fake) |>
    dplyr::summarise(stress = mean(stress), cope = mean(cope), .by = record_id)

  expect_lt(stats::cor(intercepts$stress, intercepts$cope), -0.3)
})

test_that("score_scales scores each subscale", {
  fake <- synthesize(two_factor_spec(), n = 10, seed = 3, quiet = TRUE)
  scored <- score_scales(fake)

  expect_true(all(c("stress_score", "cope_score") %in% names(scored)))
  expect_false("pss_score" %in% names(scored))
})

test_that("the checker catches impossible or unusable factor correlations", {
  expect_true(any(grepl("between -1 and 1", check_spec(two_factor_spec(r = 1.2))$message)))

  no_intercept <- two_factor_spec()
  no_intercept$latents$cope$formula <- "cope ~ 1 + (0 + time | record_id)"
  no_intercept$latents$cope$ranef <- list(record_id = list(sd = 0.1))
  expect_true(any(grepl("needs a random intercept", check_spec(no_intercept)$message)))

  expect_false(any(check_spec(two_factor_spec())$level == "error"))
})

test_that("expect: names a subscale for a multi-factor block", {
  spec <- two_factor_spec()
  spec$measurement$pss$expect <- list(alpha = list(value = 0.8, tol = 0.1))
  issues <- check_spec(spec, plausibility = TRUE)
  expect_true(any(grepl("subscales", issues$message)))
})

test_that("the scorify scoresheet reverses negative loadings and sums subscales", {
  path <- tempfile(fileext = ".csv")
  sheet <- write_scorify_scoresheet(example_path("calm_minds.yaml"), path)

  expect_true(file.exists(path))
  expect_equal(sheet$d[sheet$b == "pss_4"], "reverse_0_4")
  expect_equal(sheet$d[sheet$b == "pss_1"], "")
  expect_equal(sheet$c[sheet$b == "reverse_0_4"], "map(0:4,4:0)")

  # Within the coping subscale, pss_4 loads positively, so it isn't reversed.
  sheet <- write_scorify_scoresheet(two_factor_spec(), path)
  expect_equal(sheet$d[sheet$b == "pss_4"], "")
  expect_true(all(c("stress_score", "cope_score") %in% sheet$b[sheet$a == "measure"]))
})
