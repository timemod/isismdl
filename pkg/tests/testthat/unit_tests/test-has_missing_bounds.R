library(testthat)
library(regts)

test_that("has_missing_bounds works correctly", {
  # No missing bounds
  expect_false(isismdl:::has_missing_bounds(as.period_range("2010/2015")))

  # NA start bound
  p_na_start <- as.period_range("2010/2015")
  p_na_start[1] <- NA
  expect_true(isismdl:::has_missing_bounds(p_na_start))

  # NA end bound
  p_na_end <- as.period_range("2010/2015")
  p_na_end[2] <- NA
  expect_true(isismdl:::has_missing_bounds(p_na_end))

  # Both bounds NA
  p_na_both <- as.period_range("2010/2015")
  p_na_both[1] <- NA
  p_na_both[2] <- NA
  expect_true(isismdl:::has_missing_bounds(p_na_both))
})
