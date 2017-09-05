library(isismdl)
library(testthat)
library(readr)

context("user language functions (2)")

test_that("output function isis_mdl is correct", {
  msg <- "Model not correct \\(see output\\)"
  report <- capture_output(expect_error(mdl <- isis_mdl("mdl/ulfunc2"), msg))
  #cat(report)
  expect_equal_to_reference(report, "expected_output/ulfunc2.rds")
})
