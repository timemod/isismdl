library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/solve_report_tools.R")

mdl <- isis_mdl("mdl/singular2.mdl", period = "2011", silent = TRUE)
mdl$set_values(0)
mdl$set_solve_options(maxiter = 1, svdtest_tol = 1e-6)


test_that("solve", {

  expect_warning(
    report <- capture.output(mdl$solve()),
    "Simulation stopped"
  )

  expect_known_report(report,
                      "expected_output/singular2_rep1.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)
})
