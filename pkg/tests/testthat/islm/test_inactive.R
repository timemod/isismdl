library(testthat)
library(isismdl)

rm(list = ls())

context("inactive equations for ISLM model")

mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)
all_eqs <- mdl$get_eq_names()
frml_names <- c("c", "i", "md", "t")
endo_names <- mdl$get_endo_names()

test_that("Testing equation status after clone", {
  expect_identical(mdl$get_eq_names(status = "inactive"), character(0))
  mdl2 <<- mdl$clone(deep = TRUE)
  mdl2$set_eq_status("inactive", names = "c")
  expect_identical(mdl$get_eq_names(status = "inactive"), character(0))
  expect_identical(mdl2$get_eq_names(status = "inactive"), "c")
  expect_identical(mdl2$get_eq_names(status = "active"), setdiff(all_eqs, "c"))
  expect_identical(mdl2$get_endo_names(type = "frml", status = "all"),
                   frml_names)
  expect_identical(mdl2$get_endo_names(type = "frml"), setdiff(frml_names, "c"))
  expect_identical(mdl2$get_endo_names(status = "inactive", type = "frml"), "c")
  expect_identical(mdl2$get_endo_names(status = "all"), endo_names)
  expect_identical(mdl2$get_endo_names(status = "active"),
                   setdiff(endo_names, "c"))
  expect_identical(mdl2$get_endo_names(status = "inactive"), "c")
  expect_identical(mdl2$get_exo_names(), c("c", "g", "ms"))
  expect_identical(mdl2$get_var_names(type = "lags"), c("r", "y", "yd"))
  expect_identical(mdl2$get_var_names(type = "lags", pattern = "^y"),
                   c("y", "yd"))
  expect_identical(mdl2$get_var_names(type = "lead"), character(0))
})

test_that("pattern argument works correctly", {
  mdl2 <<- mdl$clone(deep = TRUE)
  mdl2$set_eq_status("inactive")
  expect_identical(mdl2$get_eq_names(status = "inactive"), all_eqs)

  expect_identical(mdl2$get_endo_names(type = "feedback"), character(0))
  expect_identical(mdl2$get_endo_names(type = "feedback", status = "active"),
                   character(0))
  expect_identical(mdl2$get_endo_names(type = "feedback", status = "inactive"),
                   c("r", "y"))
  expect_identical(mdl2$get_endo_names(type = "feedback", status = "all"),
                   c("r", "y"))

  expect_identical(mdl2$get_endo_names(type = "lags"), character(0))
  expect_identical(mdl2$get_endo_names(type = "lags", status = "active"),
                   character(0))
  expect_identical(mdl2$get_endo_names(type = "lags", status = "inactive"),
                   c("r", "y", "yd"))
  expect_identical(mdl2$get_endo_names(type = "lags", status = "all"),
                   c("r", "y", "yd"))

  expect_identical(mdl2$get_endo_names(type = "leads", status = "all"),
                   character(0))

  mdl2$set_eq_status("active", pattern = "^y")
  expect_identical(mdl2$get_eq_names(status = "active"), c("y", "yd"))
  expect_identical(mdl2$get_eq_names(status = "inactive"),
                                    setdiff(all_eqs, c("y", "yd")))
  expect_identical(mdl2$get_endo_names(type = "feedback"), "y")
  expect_identical(mdl2$get_endo_names(type = "feedback", status = "all"),
                   c("r", "y"))
  expect_identical(mdl2$get_endo_names(type = "feedback", status = "inactive"),
                   c("r"))
})

test_that("Testing equation status after reading the model", {
  mdl2 <<- mdl$clone(deep = TRUE)
  mdl2$set_eq_status("inactive", names = "c")
  mdl2$write_mdl("temp.ismdl")
  capture.output(mdl3 <- read_mdl("temp.ismdl"))
  unlink("temp.ismdl")
  expect_identical(mdl3$get_eq_names(status = "inactive"), "c")
})

test_that("behaviour of deactivated equations", {
  mdl2 <- mdl$copy()
  mdl2$set_eq_status("inactive", names = "c")
  expect_equal(mdl2$get_simul_names(), c("i", "md", "t", "yd", "y", "r"))
  mdl2$set_values(600, names = "c")
  c_ref <- mdl2$get_data(names = "c")
  mdl2$solve(options = list(report = "none"))
  expect_equal(mdl2$get_data(names = "c"), c_ref)
  expect_equal(mdl2$get_ca(), mdl$get_ca())

  # now make all equations active again, and solve
  mdl2$set_eq_status("active")
  mdl2$solve(options = list(report = "none"))
  expect_equal(mdl2$get_data(period = "2015Q2/"),
               mdl$get_data(period = "2015Q2/"))

  mdl$set_eq_status("active", names = c("c", "i"))
  expect_equal(mdl2$get_simul_names(), c("i", "md", "t", "yd", "c", "y", "r"))
})

test_that("deactivating fixed equations", {
  mdl2 <- mdl$copy()
  c_ref <- mdl2$get_data(names = "c")
  mdl2$set_fix_values(650, names = "c", period = "/2015Q3")
  c_ref["/2015Q3", ] <- 650
  expect_equal(mdl2$get_data(names = "c"), c_ref)
  mdl2$set_eq_status("inactive", names = "c")
  mdl2$solve(options = list(report = "none"))
  expect_equal(mdl2$get_data(names = "c"), c_ref)
  expect_equal(mdl2$get_ca(names = "c"), mdl$get_ca(names = "c"))
  expect_error(mdl2$run_eqn(names = "c"),
               "\"c\" is not an active equation")
  expect_equal(mdl2$get_data(names = "c"), c_ref)
  expect_equal(mdl2$get_ca(names = "c"), mdl$get_ca(names = "c"))
})

test_that("fixing deactived equation give an error", {
  mdl2 <- mdl$copy()
  mdl2$set_eq_status("inactive", names = c("c", "i"))
  msg <- "Variable c is inactive and cannot be fixed"
  expect_error(mdl2$set_fix_values(650, names = "c", period = "/2015Q3"),
               msg)
  data <- mdl2$get_data(names = c("c", "i"))
  msg <- "The variables c i are inactive and cannot be fixed"
  expect_error(mdl2$set_fix(data), msg)
})

test_that("deactivating fitted equations", {
  mdl2 <- mdl$copy()
  y_ref <- mdl2$get_data(names = "y")
  mdl2$set_fit_values(10000, names = "y", period = "/2015Q3")
  mdl2$set_rms(c(c = 5.0))
  mdl2$set_eq_status("inactive", names = "y")
  expect_warning(mdl2$solve(options = list(report = "none")),
                 "Simulation not possible")
  expect_equal(mdl2$get_data(names = "y"), y_ref)
  expect_equal(mdl2$get_ca(names = "c"), mdl$get_ca(names = "c"))
})

test_that("fixing deactived equation give an error", {
  mdl2 <- mdl$copy()
  mdl2$set_eq_status("inactive", names = c("y", "yd"))
  msg <- "Variable y is inactive and cannot be used as fit target"
  expect_error(mdl2$set_fit_values(650, names = "y", period = "/2015Q3"),
               msg)
  data <- mdl2$get_data(names = c("y", "yd"))
  msg <- "The variables y yd are inactive and cannot be used as fit targets"
  expect_error(mdl2$set_fit(data), msg)
})

test_that("errors", {
  mdl2 <- mdl$copy()
  expect_error(mdl2$set_eq_status(status = "inactive", names = c("y", "yyy")),
               "\"yyy\" is not an equation.")
  expect_error(mdl2$set_eq_status(status = "active", names = c("y", "yyy", "x")),
                "The following names are no equations: \"yyy\", \"x\".")
  expect_warning(mdl2$set_eq_status(status = "active", names = "y", pattern = "^z"),
                "There are no equations that match pattern '\\^z'.")
})
