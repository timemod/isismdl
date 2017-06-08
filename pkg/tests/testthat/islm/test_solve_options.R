context("solve options for ISLM model")

library(testthat)
library(isismdl)
library(utils)

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
})

test_that("errors", {

  msg <- "The solve_options should be specified as named arguments"
  expect_error(islm_model$set_solve_options("xxx"), msg)
  expect_error(islm_model$set_solve_options("xxx",  maxiter = 2), msg)

  msg <- "Unknown solve option x"
  expect_error(islm_model$set_solve_options(x = "xxx"), msg)

  msg <- "Illegal value xxx for option report"
  expect_error(islm_model$set_solve_options(report = "xxx"), msg)
})
