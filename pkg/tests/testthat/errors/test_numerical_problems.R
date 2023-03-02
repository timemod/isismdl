library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE


source("../tools/convert_report.R")

mdl <- isis_mdl("mdl/numerical_problems", period = "2011Y", silent = TRUE)
mdl$set_values(1, names = "x")
mdl$set_values(0, names = "y")
mdl$set_debug_eqn(TRUE)

test_that("output is correct", {
  report <- convert_report(capture.output(mdl$solve()))
  expected_output_file <- "expected_output/numerical_problems.txt"
  # For R version < 4.2.0, there is a difference between the result on Windows
  # and Linux, probably because of a different Fortran compiler.
  # On Windows, Fortran expression 'max(1, NaN)' yields 1 for R version < 4.2.0
  # (this is not correct). On Linux and for R >= 4.2.0 we get NaN, which is
  # correct.
  if (.Platform$OS.type == "windows" && getRversion() < "4.2.0") {
    expected_output_file <- "expected_output/numerical_problems_windows_before_R_4_2.txt"
  }
  expect_known_output(cat_report(report), expected_output_file,
                      update = update_expected)
})
