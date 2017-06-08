library(testthat)
library(isismdl)
library(utils)

context("solve options for ISLM model")

capture_output(islm_model <- read_mdl("islm_model.rds"))

test_that("get_solve_options / set_solve_options", {
    default_opts <- islm_model$get_solve_options()
    expect_equal_to_reference(default_opts, file = "data/default_solve_opts.Rds")
    opts <- default_opts
    opts["mode"] <- "reschk"
    opts["cnmtrx"] <- 0.7
    opts["xupdate"] <- "lastval"
    expect_identical(do.call(islm_model$set_solve_options, opts), islm_model)
    expect_identical(islm_model$get_solve_options(), opts)

    islm_model$set_solve_options(dbgopt = c("priscal", "prifb"))

    expect_identical(islm_model$get_solve_options()[["dbgopt"]],
                     c("prifb", "noprild", "noprijac", "noprinoconv",
                       "noprinotconvl", "priscal"))

    msg <- "The minimum of xtfac is 2"
    expect_warning(islm_model$set_solve_options(xtfac = 1))
    expect_identical(islm_model$get_solve_options()[["xtfac"]], 2)
})

test_that("errors", {

  msg <- "The solve_options should be specified as named arguments"
  expect_error(islm_model$set_solve_options("xxx"), msg)
  expect_error(islm_model$set_solve_options("xxx",  maxiter = 2), msg)

  msg <- "Unknown solve option x"
  expect_error(islm_model$set_solve_options(x = "xxx"), msg)

  msg <- "Illegal value xxx for option report"
  expect_error(islm_model$set_solve_options(report = "xxx"), msg)

  msg <- "The value for option maxiter should have length 1"
  expect_error(islm_model$set_solve_options(maxiter = c("10", "20")))

  msg <- "maxiter should be an integer"
  expect_error(islm_model$set_solve_options(maxiter = "xxx"), msg)
  expect_error(islm_model$set_solve_options(maxiter = 2.12), msg)

  msg <- "maxiter should be a non-negative integer"
  expect_error(islm_model$set_solve_options(maxiter = -10), msg)

  msg <- "maxiter should not be NA"
  expect_error(islm_model$set_solve_options(maxiter = NA), msg)

  msg <- "cnmtrx should be a number"
  expect_error(islm_model$set_solve_options(cnmtrx = "xxx"))

  msg <- "cnmtrx should be a finite number"
  expect_error(islm_model$set_solve_options(cnmtrx = NA))

  msg <- "rlxmin should be smaller than or equal to 1"
  expect_error(islm_model$set_solve_options(rlxmin = 2))

  msg <- "ratreport_rep should be a numeric vector"
  expect_error(islm_model$set_solve_options(ratreport_rep = "jan"), msg)

  msg <- "ratreport_rep should not contain NA values"
  expect_error(islm_model$set_solve_options(ratreport_rep = c(1, NA)), msg)

  msg <- "rlxmin is smaller than rlxmax"
  expect_error(islm_model$set_solve_options(rlxmin = 0.4, rlxmax = 0.2), msg)
})
