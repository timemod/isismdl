library(utils)
library(testthat)
library(isismdl)

context("modify parameters ISLM model")

rm(list = ls())

islm_model <- read_mdl("islm_model.ismdl", silent = TRUE)

islm_model$set_param(list(c0 = 120L, t1 = 0.25))

report <- islm_model$solve(options = list(report = "none"))
#print(islm_model$get_data())

isis_result <- as.regts(read.csv("isi/param.csv"), time_column = 1)
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)

test_that("Comparing results of ordinary solve after changing parameters", {
  expect_identical(islm_model$get_data_period(), period_range("2015Q1", "2016Q3"))
  expect_identical(islm_model$get_var_names(), colnames(isis_result))
  expect_identical(islm_model$get_endo_names(type = "frml"),
                     c("c", "i", "md", "t"))
  expect_identical(dif$missing_names1, character(0))
  expect_identical(dif$missing_names2, character(0))
  expect_identical(dif$difnames, character(0))
})


test_that("get_param", {
  expect_identical(islm_model$get_param(names = "c0"), list(c0 = 120))
  expect_identical(islm_model$get_param(names = "t1"), list(t1 = 0.25))
  expect_identical(islm_model$get_param(pattern = "^c."),
                     list(c0 = 120, c1 = 0.7, c2 = 20, c3 = 0.5))
})

test_that("warnings and errors for set_param", {
  expect_error(islm_model$set_param(2), "Argument p has no names")
  expect_error(islm_model$set_param(list(2,3)), "Argument p has no names")
  expect_warning(islm_model$set_param(list(c0 = 120, xx = c(1,2))),
                 "\"xx\" is not a parameter")
  expect_error(islm_model$set_param(list(c0 = 120, xx = c(1,2)), name_err = "stop"),
                 "\"xx\" is not a parameter")
  expect_error(islm_model$set_param(list(c0 = c(120, 120))),
                 "Value for parameter c0 has an incorrect length. Required length: 1. Actual length: 2")
  expect_error(islm_model$set_param(list(c0 = "xxx", i1 = 2)),
               "c0 is not numeric")
  expect_error(islm_model$set_param(list(c0 = "xxx", i1 = FALSE)),
               "c0 and i1 are not numeric")
})

test_that("p as a named numeric vector", {
  expect_silent(islm_model$set_param(c(c0 = 240L, t1 = 0.5, xxx = 2),
                                     name_err = "silent"))
  expect_equal(islm_model$get_param(names = c("c0", "t1")),
               list(c0 = 240, t1 = 0.5))
})

test_that("set_param_values", {
  par <- islm_model$get_param()

  islm_model$set_param_values(2)
  expected_result <- par
  expected_result[] <- 2
  expect_equal(islm_model$get_param(), expected_result)

  islm_model$set_param_values(3, pattern = "^i\\d", names = "c0")
  expected_result[c("i0", "i1", "i2", "i3", "c0")] <- 3
  expect_equal(islm_model$get_param(), expected_result)

  islm_model$set_param_values(NA, pattern = "^c\\d")
  expected_result[c("c0", "c1", "c2", "c3")] <- NA_real_
  expect_equal(islm_model$get_param(), expected_result)

  islm_model$set_param_values(111, names = c("m1"))
  expected_result["m1"] <- 111
  expect_equal(islm_model$get_param(), expected_result)

  expect_silent(islm_model$set_param_values(999, pattern = "^x"))
  expect_equal(islm_model$get_param(), expected_result)

  #
  # errors
  #
  expect_error(islm_model$set_param_values("xxx"),
               "Argument 'value' should be a numeric vector")
  expect_error(islm_model$set_param_values(1:2),
               "Value for parameter c0 has an incorrect length. Required length: 1. Actual length: 2")
  expect_error(islm_model$set_param_values(3, names = "xxx"),
               "\"xxx\" is not a parameter.")
})
