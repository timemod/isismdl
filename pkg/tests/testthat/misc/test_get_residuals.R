library(testthat)
library(isismdl)
library(tibble)

# Remove all objects from the global environment to ensure a clean start
rm(list = ls())

simple_model_text <- "ident y = 0.5 * y(-1) + x;"

test_that("get_residuals basic functionality", {
  period <- as.period_range("2010/2015")
  mdl <- isis_mdl(model_text = simple_model_text, period = period, silent = TRUE)

  # Set some data
  mdl$set_values(10, names = "x", period = "2011")
  mdl$set_values(5, names = "y", period = "2010") # Lag for 2011
  mdl$set_values(12.5, names = "y", period = "2011") # Correct solved value (0.5*5 + 10)

  # 1. Test structure
  res <- mdl$get_residuals(period = "2011", fun = function(x1, x2) (x1 - x2))
  expect_s3_class(res, "data.frame")
  expect_true(all(c("name", "period", "dif", "value_derived", "value_reschk") %in% colnames(res)))

  # 2. Test accuracy (no difference)
  expect_equal(nrow(res), 0) # Because dif is 0, filtered out by default 1e-2

  # 3. Test induced difference
  mdl$set_values(15, names = "y", period = "2011") # Induce 2.5 difference
  res <- mdl$get_residuals(period = "2011", fun = function(x1, x2) (x1 - x2))
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "y")
  expect_equal(res$period, "2011")
  expect_equal(res$dif, 2.5)
  expect_equal(res$value_derived, 15)
  expect_equal(res$value_reschk, 12.5)
})

test_that("get_residuals handles NA and custom tolerance", {
  period <- as.period_range("2010/2015")
  mdl <- isis_mdl(model_text = simple_model_text, period = period, silent = TRUE)

  # Induce NA
  mdl$set_values(NA, names = "y", period = "2011")
  expect_message(res <- mdl$get_residuals(period = "2011", fun = function(x1, x2) (x1 - x2)), "resulted in missing values")
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$dif))
  expect_true(is.na(res$value_derived))

  # Test custom tolerance
  # Setup: correct value is 12.5
  mdl$set_values(5, names = "y", period = "2010")
  mdl$set_values(10, names = "x", period = "2011")
  mdl$set_values(12.6, names = "y", period = "2011") # 0.1 difference

  # Default tolerance is 1e-2 (0.01)
  res_default <- mdl$get_residuals(period = "2011", fun = function(x1, x2) (x1 - x2))
  expect_equal(nrow(res_default), 1)

  # Higher tolerance
  res_loose <- mdl$get_residuals(period = "2011", tol = 0.2, fun = function(x1, x2) (x1 - x2))
  expect_equal(nrow(res_loose), 0)

  # Lower tolerance
  mdl$set_values(12.501, names = "y", period = "2011") # 0.001 difference
  res_strict <- mdl$get_residuals(period = "2011", tol = 0.0005, fun = function(x1, x2) (x1 - x2))
  expect_equal(nrow(res_strict), 1)
  expect_equal(res_strict$dif, 0.001, tolerance = 1e-7)
})
