library(testthat)
library(isismdl)

library(utils)

rm(list = ls())


mdl <- read_mdl("islm_model.ismdl", silent  = TRUE)

default_opts <- mdl$get_fit_options()

test_that("test some default options", {
  expect_equal(default_opts[["scale_method"]], "row")
  expect_equal(default_opts[["accurate_jac"]], TRUE)
  expect_equal(default_opts[["zealous"]], TRUE)
  expect_equal(default_opts[["warn_zero_row"]], FALSE)
  expect_equal(default_opts[["warn_zero_col"]], FALSE)
  expect_equal(default_opts[["chkjac"]], TRUE)
  expect_equal(default_opts[["zero_ca"]], FALSE)
})

test_that("the default options not overwritten by method solve", {
  rms_values <- c(c = 5.0, t = 2, i = 21, md = 2)
  i <- regts(200, start = '2015Q2')
  y <- regts(c(990, NA, 1010), start = '2015Q2')
  fit_targets <- cbind(y, i)
  mdl$set_rms(rms_values)
  mdl$set_fit(fit_targets)
  mdl$solve(options = list(report = "none"))
  mdl$solve(options = list(report = "none"),
            fit_options = list(cvgabs = 1e-4, maxiter = 12, report = "fullrep",
                               dbgopt = "prica"))
  expect_identical(mdl$get_fit_options(), default_opts)
})

test_that("get_fit_options / set_fit_options", {

  mdl2 <- mdl$clone(deep = TRUE)

  opts <- default_opts
  opts["maxiter"] <- 20L
  opts["report"] <- "minimal"
  opts["warn_ca"] <- FALSE
  opts["zero_ca"] <- TRUE
  opts["cvgrel"] <- 666
  opts["scale_method"] <- "none"
  opts["chkjac"] <- FALSE
  opts["warn_zero_row"] <- TRUE
  opts["warn_zero_col"] <- TRUE

  expect_identical(do.call(mdl2$set_fit_options, opts), mdl2)
  expect_identical(mdl2$get_fit_options(), opts)
  expect_identical(mdl$get_fit_options(), default_opts)

  mdl2$set_fit_options(dbgopt = c("prica", "nosupsot"))
  expect_identical(mdl2$get_fit_options()[["dbgopt"]],
                   c("prica", "noprijac", "nosupsot"))
  expect_false(mdl2$get_fit_options()[["warn_ca"]])
  expect_true(mdl2$get_fit_options()[["zero_ca"]])

  opts <- mdl2$get_fit_options()
  mdl3 <- mdl2$clone(deep = TRUE)
  do.call(mdl3$set_fit_options, opts)
  expect_identical(mdl3$get_fit_options(), opts)

  mdl2$write_mdl("temp.ismdl")
  capture_output(mdl4 <- read_mdl("temp.ismdl"))
  unlink("temp.ismdl")
  expect_identical(mdl4$get_fit_options(), opts)
})

test_that("errors", {

  mdl2 <- mdl$clone(deep = TRUE)

  msg <- "maxiter should be an integer"
  expect_error(mdl2$set_fit_options("aap"), msg)
  expect_error(mdl2$set_fit_options("xxx",  cvgabs = 2), msg)

  msg <- "Illegal value xxx for option report"
  expect_error(mdl2$set_fit_options(report = "xxx"), msg)

  msg <- "The value for option maxiter should have length 1"
  expect_error(mdl2$set_fit_options(maxiter = c("10", "20")))

  msg <- "maxiter should be an integer"
  expect_error(mdl2$set_fit_options(maxiter = "xxx"), msg)
  expect_error(mdl2$set_fit_options(maxiter = 2.12), msg)

  msg <- "maxiter should be a positive integer"
  expect_error(mdl2$set_fit_options(maxiter = -10), msg)

  msg <- "maxiter should not be NA"
  expect_error(mdl2$set_fit_options(maxiter = NA), msg)

  msg <- "zero_ca should be a logical"
  expect_error(mdl2$set_fit_options(zero_ca = 4), msg)

  msg <- "warn_ca should not be NA"
  expect_error(mdl2$set_fit_options(warn_ca = NA), msg)

  msg <- "chkjac should be a logical"
  expect_error(mdl2$set_fit_options(chkjac = 1), msg)

  msg <- "maxiter should be a positive integer"
  expect_error(mdl2$set_fit_options(maxiter = 0), msg)

  msg <- "maxiter should be an integer"
  expect_error(mdl2$set_fit_options(maxiter = 1.2), msg)
})
