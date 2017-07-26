library(utils)
library(isismdl)
library(testthat)

context("solve ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

mdl$set_solve_options(report = "none")

test_that("warnings", {
  mdl2 <- mdl$copy()
  mdl2$set_values(NA, names = "y", period = "2015Q2")
  expect_warning(mdl2$solve(), "Simulation not possible")
  expect_warning(mdl2$solve(period = "2015Q3"),
                 "Initial lags/leads missing/invalid. Simulation not possible")
  mdl2$set_values(NA, names = "g", period = "2015Q4")
  expect_warning(mdl2$solve(period = "2015Q4"), "Simulation not possible")
  mdl2$set_param(list(c0 = NA_real_))
  expect_warning(mdl2$solve(),
          "Invalid parameter values detected. Simulation not possible")
})

test_that("no warnings for erropt cont", {
  mdl2 <- mdl$copy()
  mdl2$set_solve_options(erropt = "cont")
  mdl2$set_values(NA, names = "y", period = "2015Q2")
  expect_warning(mdl2$solve(), "Simulation not possible")
  expect_warning(mdl2$solve(period = "2015Q3"), "Simulation stopped")
  mdl2$set_values(NA, names = "g", period = "2015Q4")
  expect_warning(mdl2$solve(period = "2015Q4"), "Simulation stopped")
  mdl2$set_param(list(c0 = NA_real_))
  expect_warning(mdl2$solve(),
                 "Invalid parameter values detected. Simulation not possible")
})
