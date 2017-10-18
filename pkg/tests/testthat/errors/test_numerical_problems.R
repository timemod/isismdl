library(isismdl)
library(testthat)

context("numerical problems")

source("../tools/convert_report.R")

capture_output(mdl <- isis_mdl("mdl/numerical_problems", period = "2011Y"))
mdl$set_values(1, names = "x")
mdl$set_values(0, names = "y")
mdl$set_debug_eqn(TRUE)

test_that("output is correct", {
  report <- convert_report(capture.output(mdl$solve()))
  #print(report)
  expected_report_file <- "expected_output/numerical_problems.rds"
  expect_equal_to_reference(report, expected_report_file)
  #old_report <- readRDS(expected_report_file)
  #cat(old_report)
  #writeLines(old_report, "test1.txt")
  #writeLines(report, "test2.txt")
})
