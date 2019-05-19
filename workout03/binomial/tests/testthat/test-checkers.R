context("Check checker functions")

test_that("check prob tests", {
  expect_true(check_prob(0.2))
  expect_error(check_prob(-1))
  expect_error(check_prob(c(.11, .32)))
})

test_that("check trials tests", {
  expect_true(check_trials(4))
  expect_error(check_trials(-1))
  expect_error(check_trials(c(5, 3)))
})

test_that("check success tests", {
  expect_true(check_success(4,5))
  expect_error(check_success(-1, 5))
  expect_error(check_prob(c(5, 3)))
})

