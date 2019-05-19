context("Check binomial functions")

test_that("check choose tests", {
  expect_true(bin_choose(5,4) == choose(5,4))
  expect_error(bin_choose(4, 5))
  expect_true(bin_choose(100, 100) == choose(100,100))
})

test_that("check probability tests", {
  expect_true(bin_probability(2, 5, 0.5) == 0.3125)
  expect_error(bin_probability(6, 5, 0.3))
  expect_error(bin_probability(6, 5, 2))
})

test_that("check distribution tests", {
  expect_true(bin_distribution(5, 0.5)[1,c("probability")] == 0.03125)
  expect_error(bin_distribution(-1, 0.5))
  expect_error(bin_distribution(3, 4))
})
