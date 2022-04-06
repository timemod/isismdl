library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- TRUE

context("numerical problems")

source("../tools/convert_report.R")

mdl <- isis_mdl("mdl/numerical_problems", period = "2011Y", silent = TRUE)
mdl$set_values(1, names = "x")
mdl$set_values(0, names = "y")
mdl$set_debug_eqn(TRUE)

test_that("output is correct", {
  report <- convert_report(capture.output(mdl$solve()))
  #print(report)
  # there is a difference between the result on Window and Linux, probably
  # because of a different fortran compiler.
  # On Windows, Fortran expression 'max(1, NaN)' yields 1 (this is not correct),
  # on Linux we get NaN, which is correct.
  expected_output_file <- sprintf("expected_output/numerical_problems_%s.txt",
                                  .Platform$OS.type)
  expect_known_output(cat_report(report), expected_output_file,
                      update = update_expected)
})
