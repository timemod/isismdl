library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

context("singular model 2")

source("../tools/convert_report.R")

mdl <- isis_mdl("mdl/singular2.mdl", period = "2011", silent = TRUE)
mdl$set_values(0)
mdl$set_solve_options(maxiter = 1, svdtest_tol = 1e-6)


test_that("solve", {

  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")

  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/singular2_rep1.txt",
                      update = update_expected)

})
