library(isismdl)
library(testthat)

rm(list = ls())

context("numerical problems")

source("../tools/convert_report.R")

mdl <- isis_mdl("mdl/numerical_problems", period = "2011Y", silent = TRUE)
mdl$set_values(1, names = "x")
mdl$set_values(0, names = "y")
mdl$set_debug_eqn(TRUE)

test_that("output is correct", {
  report <- convert_report(capture.output(mdl$solve()))
  #print(report)
  expect_known_output(cat_report(report), "expected_output/numerical_problems.txt")
})
