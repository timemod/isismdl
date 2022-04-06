library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

context("singular model 1")

source("../tools/convert_report.R")

mdl <- isis_mdl("mdl/singular1.mdl", period = "2011", silent = TRUE)
mdl$set_values(0)
mdl$set_solve_options(dbgopt = c("prifb", "prijac"), maxiter = 1,
                       svdtest_tol = 1e-10)


test_that("solve", {

  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")

  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/singular1_rep1.txt",
                      update = update_expected)

  # No output when solve options report ="none":
  mdl$set_values(0)
  expect_silent(expect_warning(mdl$solve(options = list(report = "none",
                                                        svdtest_tol = 1e-6)),
                 "Simulation stopped"))
  expect_identical(mdl$get_solve_status(), "Simulation stopped")
})
