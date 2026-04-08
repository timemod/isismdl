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
  expect_true(all(c("name", "period", "dif", "value_lhs", "value_rhs") %in% colnames(res)))

  # Induce difference: LHS = 15, RHS = 12.5 -> dif = 2.5
  mdl$set_values(15, names = "y", period = "2011")
  res <- mdl$get_residuals(period = "2011", fun = function(x1, x2) (x1 - x2))
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "y")
  expect_equal(res$period, "2011")
  expect_equal(res$dif, 2.5)
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
                 "De volgende rhs variabelen konden niet worden berekend", fixed = TRUE)
  expect_equal(nrow(res_rhs_na), 0)

  # Case 2: LHS is NA (y is missing)
  # RHS is calculable (12.5). The result should include it with NA difference.
  mdl$set_values(10, names = "x", period = "2011") # Restore x
  mdl$set_values(NA, names = "y", period = "2011")
  expect_message(res_lhs_na <- mdl$get_residuals(period = "2011"),
                 "Voor de volgende lhs variabelen ontbreken waarnemingen", fixed = TRUE)
  expect_equal(nrow(res_lhs_na), 1)
  expect_true(is.na(res_lhs_na$dif))
  expect_true(is.na(res_lhs_na$value_lhs))
  expect_equal(res_lhs_na$value_rhs, 12.5)

  # Case 3: Both NA
  mdl$set_values(NA, names = "x", period = "2011")
  mdl$set_values(NA, names = "y", period = "2011")
  expect_message(res_both_na <- mdl$get_residuals(period = "2011"),
                 "(?s)Voor de volgende lhs variabelen ontbreken waarnemingen.*De volgende rhs variabelen konden niet worden berekend",
                 perl = TRUE)
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
  mdl$set_values(22, names = "y2", period = "2011") # diff 1 (should be 21)

  res <- mdl$get_residuals(period = "2011")
  expect_equal(nrow(res), 1)
  expect_equal(res$name, "y2")

  # Test custom tolerance
  mdl$set_values(11.05, names = "y1", period = "2011") # diff 0.05
  res_default <- mdl$get_residuals(period = "2011") # tol = 0.01
  expect_equal(nrow(res_default), 2)

  res_loose <- mdl$get_residuals(period = "2011", tol = 0.1)
  expect_equal(nrow(res_loose), 1) # Only y2 (diff 1)
  expect_equal(res_loose$name, "y2")

  # Test with one failing (missing x2)
  mdl$set_values(NA, names = "x2", period = "2011")
  expect_message(res_mixed <- mdl$get_residuals(period = "2011", tol = 0.01),
                 "De volgende rhs variabelen konden niet worden berekend", fixed = TRUE)
  expect_equal(nrow(res_mixed), 1) # Only y1 (diff 0.05)
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
    "(?s)Voor de volgende lhs variabelen ontbreken waarnemingen.*De volgende rhs variabelen konden niet worden berekend",
    perl = TRUE
  )

  # Check final table
  # y1: 2011 (LHS NA) -> in table
  # y1: 2012 -> not in table (dif=0)
  # y2: 2011 -> not in table (dif=0)
  # y2: 2012 (RHS NA) -> NOT in table
  # y3: 2011 (Both NA) -> NOT in table (because RHS NA is filtered out)
  # y3: 2012 -> not in table (dif=0)

  expect_equal(nrow(res), 1)
  expect_equal(res$name, "y1")
  expect_equal(res$period, "2011")
  expect_true(is.na(res$dif))
})
