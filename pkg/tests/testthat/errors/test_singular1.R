library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/solve_report_tools.R")

mdl <- isis_mdl("mdl/singular1.mdl", period = "2011", silent = TRUE)
mdl$set_values(0)
mdl$set_solve_options(dbgopt = c("prifb", "prijac"), maxiter = 1)

test_that("solve", {

  expect_warning(
    report <- capture.output(mdl$solve()),
    "Simulation stopped"
  )
  expect_known_report(report,
                      "expected_output/singular1_rep1.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)

  # No output when solve options report ="none":
  mdl$set_values(0)
  expect_silent(
    expect_warning(
      mdl$solve(options = list(report = "none")),
      "Simulation stopped"
    )
  )
  expect_identical(mdl$get_solve_status(), "Simulation stopped")
})
