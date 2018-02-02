library(testthat)
library(isismdl)
library(utils)

context("solve options for ISLM model")

capture_output(mdl <- read_mdl("islm_model.ismdl"))

default_opts <- mdl$get_solve_options()

test_that("default option the same as before", {
  expect_known_value(default_opts,
                      file = "expected_output/default_solve_opts.rds")
})

test_that("the default options not overwritten by method solve", {
  mdl$solve(options = list(report = "none"))
  mdl$solve(options = list(mode = "reschk", maxiter = 10, report = "none"))
  expect_identical(mdl$get_solve_options(), default_opts)
})

test_that("get_solve_options / set_solve_options", {

  mdl2 <- mdl$clone(deep = TRUE)

  opts <- default_opts
  opts["mode"] <- "reschk"
  opts["cnmtrx"] <- 0.7
  opts["xupdate"] <- "lastval"
  expect_identical(do.call(mdl2$set_solve_options, opts), mdl2)
  expect_identical(mdl2$get_solve_options(), opts)
  expect_identical(mdl$get_solve_options(), default_opts)

  mdl2$set_solve_options(dbgopt = c("priscal", "prifb"))
  expect_identical(mdl2$get_solve_options()[["dbgopt"]],
                   c("prifb", "noprild", "noprijac", "noprinoconv",
                     "noprinotconvl", "priscal"))

  opts <- mdl2$get_solve_options()
  mdl3 <- mdl2$clone(deep = TRUE)
  do.call(mdl3$set_solve_options, opts)
  expect_identical(mdl3$get_solve_options(), opts)

  mdl2$write_mdl("temp.ismdl")
  capture_output(mdl4 <- read_mdl("temp.ismdl"))
  unlink("temp.ismdl")
  expect_identical(mdl4$get_solve_options(), opts)

  msg <- "The minimum of xtfac is 2"
  expect_warning(mdl2$set_solve_options(xtfac = 1))
  expect_identical(mdl2$get_solve_options()[["xtfac"]], 2)
})

test_that("errors", {

  mdl2 <- mdl$clone(deep = TRUE)

  msg <- "Illegal value xxx for option mode"
  expect_error(mdl2$set_solve_options("xxx"), msg)
  expect_error(mdl2$set_solve_options("xxx",  maxiter = 2), msg)

  msg <- "Illegal value xxx for option report"
  expect_error(mdl2$set_solve_options(report = "xxx"), msg)

  msg <- "The value for option maxiter should have length 1"
  expect_error(mdl2$set_solve_options(maxiter = c("10", "20")))

  msg <- "maxiter should be an integer"
  expect_error(mdl2$set_solve_options(maxiter = "xxx"), msg)
  expect_error(mdl2$set_solve_options(maxiter = 2.12), msg)

  msg <- "maxiter should be a non-negative integer"
  expect_error(mdl2$set_solve_options(maxiter = -10), msg)

  msg <- "maxiter should not be NA"
  expect_error(mdl2$set_solve_options(maxiter = NA), msg)

  msg <- "cnmtrx should be a number"
  expect_error(mdl2$set_solve_options(cnmtrx = "xxx"))

  msg <- "cnmtrx should be a finite number"
  expect_error(mdl2$set_solve_options(cnmtrx = NA))

  msg <- "rlxmin should be smaller than or equal to 1"
  expect_error(mdl2$set_solve_options(rlxmin = 2))

  msg <- "ratreport_rep should be an integer"
  expect_error(mdl2$set_solve_options(ratreport_rep = "jan"), msg)

  msg <- "ratreport_rep should not be NA"
  expect_error(mdl2$set_solve_options(ratreport_rep = NA), msg)
  expect_silent(mdl2$set_solve_options(ratfullreport_rep = NA))

  msg <- "rlxmin is smaller than rlxmax"
  expect_error(mdl2$set_solve_options(rlxmin = 0.4, rlxmax = 0.2), msg)
})
