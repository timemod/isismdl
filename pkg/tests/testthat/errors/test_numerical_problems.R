library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- TRUE

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
  data <- as_data_frame(mdl$get_data())
  data_expected <- data.frame(period = "2011")
  data_expected[, c("dum", "x", "x1", "x10", "x11", "x12", "x2", "x3", "x4",
                    "x5", "x6", "x7", "x8", "x9", "y")] <- NA_real_
  data_expected[, "x"] <- 1
  data_expected[, "x6"] <- 1.123345**4
  data_expected[, "y"] <- 0
  expect_equal(data, data_expected)
})
