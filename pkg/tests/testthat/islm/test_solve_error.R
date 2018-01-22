library(utils)
library(isismdl)
library(testthat)

context("solve errors ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))
mdl$set_solve_options(report = "none")

test_that("get_solve_status for a fresh model", {
  expect_equal(mdl$get_solve_status(), "Method solve has not yet been called")
  mdl$solve()
  expect_equal(mdl$get_solve_status(), "OK")
})

test_that("warnings", {

  mdl2 <- mdl$copy()

  mdl2$set_values(NA, names = "y", period = "2015Q2")
  msg <- "Simulation not possible"
  expect_warning(mdl2$solve(), msg)
  expect_equal(mdl2$get_solve_status(), msg)

  msg <- "Initial lags/leads missing/invalid. Simulation not possible"
  expect_warning(mdl2$solve(period = "2015Q3"), msg)
  expect_equal(mdl2$get_solve_status(), msg)

  mdl2$set_values(NA, names = "g", period = "2015Q4")
  msg <- "Simulation not possible"
  expect_warning(mdl2$solve(period = "2015Q4"), msg)
  expect_equal(mdl2$get_solve_status(), msg)

  mdl2$set_param(list(c0 = NA_real_))
  msg <- "Invalid parameter values detected. Simulation not possible"
  expect_warning(mdl2$solve(), msg)
  expect_equal(mdl2$get_solve_status(), msg)
})

test_that("no warnings for erropt cont", {

  mdl2 <- mdl$copy()
  mdl2$set_solve_options(erropt = "cont")

  msg <- "Simulation not possible"
  mdl2$set_values(NA, names = "y", period = "2015Q2")
  expect_warning(mdl2$solve(), msg)
  expect_equal(mdl2$get_solve_status(), msg)

  msg <- "Simulation stopped"
  expect_warning(mdl2$solve(period = "2015Q3"), msg)
  expect_equal(mdl2$get_solve_status(), msg)

  mdl2$set_values(NA, names = "g", period = "2015Q4")
  msg <- "Simulation stopped"
  expect_warning(mdl2$solve(period = "2015Q4"), msg)
  expect_equal(mdl2$get_solve_status(), msg)

  mdl2$set_param(list(c0 = NA_real_))
  msg <-  "Invalid parameter values detected. Simulation not possible"
  expect_warning(mdl2$solve(), msg)
  expect_equal(mdl2$get_solve_status(), msg)
})
