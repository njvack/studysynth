test_that("a drafted spec checks and synthesizes", {
  project <- read_redcap_project(xml = example_path("CalmMinds_metadata.xml"))
  draft <- draft_spec(project, redcap = list(xml = example_path("CalmMinds_metadata.xml")))

  issues <- check_spec(draft, project, smoke = FALSE)
  fake <- synthesize(draft, n = 10, quiet = TRUE)

  expect_false(any(issues$level == "error"))
  expect_equal(sum(attr(fake, "status")$status %in% c("completed", "withdrew")), 10)
})
