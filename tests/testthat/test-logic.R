test_that("equality and inequality translate to R", {
  expect_equal(redcap_logic_to_r("[a] = 1"), quote(`a` == 1))
  expect_equal(redcap_logic_to_r("[a] <> 1"), quote(`a` != 1))
  expect_equal(redcap_logic_to_r("[a] >= 1"), quote(`a` >= 1))
})

test_that("and / or combine conditions", {
  data <- data.frame(a = c(1, 1, 2), b = c(1, 2, 2))

  expect_equal(eval_redcap_logic("[a] = 1 and [b] = 2", data), c(FALSE, TRUE, FALSE))
  expect_equal(eval_redcap_logic("[a] = 2 or [b] = 2", data), c(FALSE, TRUE, TRUE))
})

test_that("checkbox references use the ___ column", {
  expect_equal(redcap_logic_to_r("[race(2)] = 1"), quote(`race___2` == 1))

  data <- data.frame(race___2 = c(0, 1))
  expect_equal(eval_redcap_logic("[race(2)] = 1", data), c(FALSE, TRUE))
})

test_that("text numbers compare equal to numbers", {
  data <- data.frame(x = c("1", "2"))

  expect_equal(eval_redcap_logic("[x] = '1'", data), c(TRUE, FALSE))
})

test_that("sum() ignores blanks, but is blank when everything is", {
  data <- data.frame(a = c(1, NA, NA), b = c(2, 3, NA))

  expect_equal(eval_redcap_logic("sum([a], [b])", data), c(3, 3, NA))
})

test_that("if() picks between values", {
  data <- data.frame(a = c(1, 5))

  expect_equal(eval_redcap_logic("if([a] > 2, 10, 20)", data), c(20, 10))
})

test_that("blanks never satisfy a comparison", {
  data <- data.frame(a = c(NA, 5))

  expect_equal(eval_redcap_logic("[a] > 2", data) %in% TRUE, c(FALSE, TRUE))
  expect_true(is.na(eval_redcap_logic("[a] = 5", data)[1]))
})

test_that("cross-event references use the current row", {
  expect_equal(redcap_logic_to_r("[baseline_arm_1][age] >= 18"), quote(`age` >= 18))
  expect_equal(
    eval_redcap_logic("[baseline_arm_1][age] >= 18", data.frame(age = c(17, 30))),
    c(FALSE, TRUE)
  )
})

test_that("logic_fields lists referenced fields once", {
  out <- logic_fields("[age] >= 18 and [race(2)] = 1 and [baseline_arm_1][age] > 0")

  expect_setequal(out, c("age", "race"))
})
