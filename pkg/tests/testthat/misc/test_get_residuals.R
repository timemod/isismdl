library(testthat)
library(isismdl)
library(tibble)

# 1. Basic Functionality
test_that("get_residuals basic functionality", {
  period <- as.period_range("2010/2015")
  mdl <- isis_mdl(model_text = "ident y = 0.5 * y(-1) + x;", period = period, silent = TRUE)

  # Setup data for 2011 (LHS = RHS = 12.5)
  mdl$set_values(10, names = "x", period = "2011")
  mdl$set_values(5, names = "y", period = "2010") # Lag for 2011
  mdl$set_values(12.5, names = "y", period = "2011")

  # Test structure and column names
  res <- mdl$get_residuals(period = "2011", fun = function(x1, x2) (x1 - x2))
  expect_s3_class(res, "data.frame")
  expect_true(all(c("name", "period", "residual", "value_lhs", "value_rhs") %in% colnames(res)))

  # Induce difference: LHS = 15, RHS = 12.5 -> residual = 2.5
  mdl$set_values(15, names = "y", period = "2011")
  res <- mdl$get_residuals(period = "2011", fun = function(x1, x2) (x1 - x2))
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "y")
  expect_equal(res$period, "2011")
  expect_equal(res$residual, 2.5)
  expect_equal(res$value_lhs, 15)
  expect_equal(res$value_rhs, 12.5)
})

# 2. NA Handling Logic
test_that("get_residuals handles NA cases separately", {
  period <- as.period_range("2010/2015")
  mdl <- isis_mdl(model_text = "ident y = 0.5 * y(-1) + x;", period = period, silent = TRUE)
  mdl$set_values(5, names = "y", period = "2010")
  mdl$set_values(10, names = "x", period = "2011")
  mdl$set_values(12.5, names = "y", period = "2011")

  # Case 1: RHS is NA (x is missing)
  # This should trigger the message and exclude the variable from the diff table
  mdl$set_values(NA, names = "x", period = "2011")
  expect_message(res_rhs_na <- mdl$get_residuals(period = "2011"),
                 "The following rhs variables could not be calculated", fixed = TRUE)
  expect_equal(nrow(res_rhs_na), 0)

  # Case 2: LHS is NA (y is missing)
  # RHS is calculable (12.5). The result should include it with NA difference.
  mdl$set_values(10, names = "x", period = "2011") # Restore x
  mdl$set_values(NA, names = "y", period = "2011")
  expect_message(res_lhs_na <- mdl$get_residuals(period = "2011"),
                 "For the following lhs variables, observations", fixed = TRUE)
  expect_equal(nrow(res_lhs_na), 0)

  # Case 3: Both NA
  mdl$set_values(NA, names = "x", period = "2011")
  mdl$set_values(NA, names = "y", period = "2011")
  expect_message(
    res_both_na <- mdl$get_residuals(period = "2011"),
    paste("(?s)For the following lhs variables, observations.*The following",
          "rhs variables could not be calculated"),
    perl = TRUE
  )
  expect_equal(nrow(res_both_na), 0)
})

# 3. Tolerance and Multiple Equations
test_that("get_residuals tolerance and multi-equation", {
  two_eq_text <- "ident y1 = x1 + 1; ident y2 = x2 + 1;"
  period <- as.period_range("2010/2015")
  mdl <- isis_mdl(model_text = two_eq_text, period = period, silent = TRUE)

  # Setup: y1 matches, y2 differs
  mdl$set_values(10, names = "x1", period = "2011")
  mdl$set_values(11, names = "y1", period = "2011")
  mdl$set_values(20, names = "x2", period = "2011")
  mdl$set_values(22, names = "y2", period = "2011")

  res <- mdl$get_residuals(period = "2011")
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "y2")

  # Test custom tolerance
  mdl$set_values(11.05, names = "y1", period = "2011") # residual 0.05
  res_default <- mdl$get_residuals(period = "2011") # tol = 0.01
  expect_equal(nrow(res_default), 2)

  res_loose <- mdl$get_residuals(period = "2011", tol = 0.1)
  expect_equal(nrow(res_loose), 1) # Only y2 (residual 1)
  expect_equal(res_loose$name, "y2")

  # Test with one failing (missing x2)
  mdl$set_values(NA, names = "x2", period = "2011")
  expect_message(res_mixed <- mdl$get_residuals(period = "2011", tol = 0.01),
                 "The following rhs variables could not be calculated", fixed = TRUE)
  expect_equal(nrow(res_mixed), 1) # Only y1 (residuals 0.05)
  expect_equal(res_mixed$name, "y1")
})

# 4. Extended tests
test_that("get_residuals extended cases", {
  two_eq_text <- "ident y1 = x1 + 1; ident y2 = x2 + 1; ident y3 = x3 + 1;"
  period <- as.period_range("2011/2012")
  mdl <- isis_mdl(model_text = two_eq_text, period = period, silent = TRUE)

  # Set some data
  mdl$set_values(10, names = c("x1", "x2", "x3"), period = "2011/2012")
  mdl$set_values(11, names = c("y1", "y2", "y3"), period = "2011/2012")

  # Induce multiple NAs
  # y1: missing data in 2011 (LHS NA)
  # y2: missing x2 in 2012 (RHS NA)
  # y3: both LHS and RHS NA in 2011
  mdl$set_values(NA, names = "y1", period = "2011")
  mdl$set_values(NA, names = "x2", period = "2012")
  mdl$set_values(NA, names = "y3", period = "2011")
  mdl$set_values(NA, names = "x3", period = "2011")

  expect_message(
    res <- mdl$get_residuals(period = "2011/2012"),
    paste("(?s)For the following lhs variables, observations.*The following",
          "rhs variables could not be calculated"),
    perl = TRUE
  )

  expect_equal(nrow(res), 0)
})

# 5. Label existence and duplication check
test_that("get_residuals includes one label column", {
  mdl <- islm_mdl(period = "2017Q1/2018Q4", silent = TRUE)
  mdl$solve(options = list(report = "none"))
  mdl$set_values(1000, names = "y", period = "2017Q2")
  res <- mdl$get_residuals(period = "2017Q2")

  expect_true("label" %in% colnames(res))
  expect_false("label_lhs" %in% colnames(res))
  expect_false("label_rhs" %in% colnames(res))
  expect_true("residual" %in% colnames(res))
  expect_equal(nrow(res), 5)

  # Check if label is correct
  expect_equal(res$label[res$name == "y"], "income")
})
