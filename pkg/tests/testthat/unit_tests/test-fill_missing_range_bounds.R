library(testthat)
library(regts)

test_that("fill_missing_range_bounds works correctly", {
  base_p <- as.period_range("2010/2015")

  # period has both bounds defined
  p_full <- as.period_range("2011/2014")
  expect_equal(isismdl:::fill_missing_range_bounds(p_full, base_p), p_full)

  # period has NULL start bound
  p_no_start <- period_range(NULL, "2014")
  expect_equal(isismdl:::fill_missing_range_bounds(p_no_start, base_p), as.period_range("2010/2014"))

  # period has NULL end bound
  p_no_end <- period_range("2011", NULL)
  expect_equal(isismdl:::fill_missing_range_bounds(p_no_end, base_p), as.period_range("2011/2015"))
})
