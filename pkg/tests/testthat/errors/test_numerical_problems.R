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
  expect_known_output(cat_report(report), expected_output_file,
                      update = update_expected)
})
