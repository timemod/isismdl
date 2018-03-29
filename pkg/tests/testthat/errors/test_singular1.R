library(isismdl)
library(testthat)

rm(list = ls())

capture_output(mdl <- isis_mdl("mdl/singular1.mdl", period = "2011"))
mdl$set_values(0)
mdl$set_solve_options(dbgopt = c("prifb", "prijac"), maxiter = 1,
                       svdtest_tol = 1e-10)

test_that("solve", {
  expect_warning(expect_output(mdl$solve(options = list(report = "none"))),
               "Simulation stopped")
  mdl$set_values(0)
  expect_silent(expect_warning(mdl$solve(options = list(report = "none",
                                                        svdtest_tol = -1)),
                 "Simulation stopped"))
  expect_identical(mdl$get_solve_status(), "Simulation stopped")
})
