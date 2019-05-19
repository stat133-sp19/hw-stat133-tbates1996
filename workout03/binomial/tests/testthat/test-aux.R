context("Check aux functions")

test_that("check aux mean tests", {
  expect_true(aux_mean(10, 0.3) == 3)
  expect_true(aux_mean(1000, 0.01) == 1000*0.01)
  expect_true(aux_mean(3, .4) == 3*0.4)
})

test_that("check aux variance tests", {
  expect_true(aux_variance(10, 0.3) == 10*0.3*.7)
  expect_true(aux_variance(1000, .01) == 1000*0.01*0.99)
  expect_true(aux_variance(3, .4) == 3*0.4*.6)
})

test_that("check aux mode tests", {
  expect_true(aux_mode(10, 0.3) == as.integer(10*0.3 + 0.3))
  expect_true(aux_mode(1000, 0.01) == as.integer(1000*0.01 + 0.01))
  expect_true(aux_mode(3, 0.8) == as.integer(3*0.8 + 0.8))
})

test_that("check aux skewness tests", {
  expect_true(aux_skewness(10, 0.3) == ((1-(2*0.3))/sqrt(10*0.3*0.7)))
  expect_true(aux_skewness(100, 0.01) == ((1-(2*0.01))/sqrt(100*0.01*0.99)))
  expect_true(aux_skewness(3, 0.7) == ((1-(2*0.7))/sqrt(3*0.7*0.3)))
})

test_that("check aux kurtosis tests", {
  expect_true(aux_kurtosis(10, 0.3) == ((1-6*0.3*0.7)/(10*0.3*.7)))
  expect_true(aux_kurtosis(100, 0.01) == ((1-6*0.01*0.99)/(100*0.01*.99)))
  expect_true(aux_kurtosis(3, 0.5) == ((1-(6*0.5*0.5))/(3*0.5*0.5)))
})

