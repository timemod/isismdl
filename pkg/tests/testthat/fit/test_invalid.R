library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/convert_report.R")

context("fit square mdl na values")

mdl <- isis_mdl("mdl/square_div_0.mdl", period = 1000, silent = TRUE)

fit <- regts(matrix(c(0.412, 0.202, 0.813, 0.642, 0.123, 0.245), nrow = 1),
             period = 1000, names = paste0("w", 1:6))

rms <- rep(1, 6)
names(rms) <- paste0("x", 1:6)

mdl$set_fit(fit)
mdl$set_rms(rms)

test_that("NA values", {
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")
  expect_known_output(cat_report(convert_report(report)),
                      "expected_output/square_invalid_rep1.txt",
                      update = update_expected)

})
