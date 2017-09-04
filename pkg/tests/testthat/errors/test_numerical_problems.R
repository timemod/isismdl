library(isismdl)
library(testthat)

context("numerical problems")

capture_output(mdl <- isis_mdl("mdl/numerical_problems", period = "2011Y"))
mdl$set_values(1, names = "x")
mdl$set_values(0, names = "y")
mdl$set_debug_eqn(TRUE)

test_that("output is correct", {
  report <- capture_output(mdl$solve())
  #cat(report)
  expect_equal_to_reference(report, "expected_output/numerical_problems.rds")
})
