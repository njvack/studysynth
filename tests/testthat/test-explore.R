fake <- synthesize(example_path("calm_minds.yaml"), n = 15, quiet = TRUE)

test_that("plots build", {
  skip_if_not_installed("ggplot2")
  plots <- list(
    plot_trajectories(fake, "phq9"), plot_items(fake, "pss", "baseline_arm_1"),
    plot_latent(fake, "dep"), plot_retention(fake), plot_ema(fake, "ema_mood")
  )

  for (p in plots) expect_no_error(ggplot2::ggplot_build(p))
})

test_that("groups are labeled with REDCap choice labels", {
  groups <- participant_groups(fake, attr(fake, "spec"), spec_project(attr(fake, "spec")))

  expect_true(all(groups$group %in% c("Waitlist control", "Mindfulness")))
})

test_that("scale_stats summarizes each event", {
  stats <- scale_stats(fake, "phq9")

  expect_setequal(names(stats), c("subscale", "event", "n", "mean", "sd", "alpha"))
  expect_true("baseline_arm_1" %in% stats$event)
})
