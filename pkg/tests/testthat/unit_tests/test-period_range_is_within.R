library(testthat)
library(regts)

test_that("period_range_is_within works correctly", {
  p_2010_2015 <- as.period_range("2010/2015")
  p_2011_2014 <- as.period_range("2011/2014")
  p_2009_2014 <- as.period_range("2009/2014")
  p_2011_2016 <- as.period_range("2011/2016")
  p_2009_2016 <- as.period_range("2009/2016")

  # period is exactly the same as long_period
  expect_true(isismdl:::period_range_is_within(p_2010_2015, p_2010_2015))

  # period is strictly within long_period
  expect_true(isismdl:::period_range_is_within(p_2011_2014, p_2010_2015))

  # period starts before long_period
  expect_false(isismdl:::period_range_is_within(p_2009_2014, p_2010_2015))

  # period ends after long_period
  expect_false(isismdl:::period_range_is_within(p_2011_2016, p_2010_2015))

  # period surrounds long_period
  expect_false(isismdl:::period_range_is_within(p_2009_2016, p_2010_2015))
})
