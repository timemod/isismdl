library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

mdl <- read_mdl("islm_model.ismdl", silent = TRUE)
mdl_solved <- read_mdl("islm_model_solved.ismdl", silent = TRUE)
isis_result <- read_ts_csv("isi/solve.csv")

test_that("mdl is not equal to mdl_solved", {
  expect_false(isTRUE(all.equal(mdl, mdl_solved)))
})

test_that("solve", {
  mdl$solve(options = list(report = "none"))

  # compare solution with Isis result
  dif <- tsdif(mdl$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
               fun = cvgdif)
  expect_true(dif$equal)
  expect_identical(dif$missing_names1, character(0))
  expect_identical(dif$missing_names2, character(0))
  expect_identical(dif$difnames, character(0))
})

test_that("Comparing ordinary solve for the ISLM model", {
  expect_true(all.equal(mdl, mdl_solved))
  expect_identical(mdl$get_data_period(),
                   period_range("2015Q1", "2016Q3"))
  expect_identical(mdl$get_var_names(), colnames(isis_result))
  expect_identical(mdl$get_endo_names(type = "frml"), c("c", "i", "md", "t"))
  expect_identical(mdl$get_endo_names(type = "feedback"), c("r", "y"))
  expect_identical(mdl$get_endo_names(type = "feedback", status = "active"),
                   c("r", "y"))
  expect_identical(mdl$get_endo_names(type = "feedback", status = "inactive"),
                   character(0))
  expect_identical(mdl$get_endo_names(type = "lags"), c("r", "y", "yd"))
  expect_identical(mdl$get_endo_names(type = "lags", status = "active"),
                   c("r", "y", "yd"))
  expect_identical(mdl$get_endo_names(type = "lags", status = "inactive"),
                   character(0))
  expect_identical(mdl$get_endo_names(type = "leads"), character(0))
  expect_identical(mdl$get_endo_names(type = "feedback", status = "inactive"),
                   character(0))
})

test_that("no upper and lower bound in period argument", {

  mdl2 <- mdl$copy()
  mdl2$set_values(999, names = "c", period = "2016Q1/2016Q3")
  mdl2$solve(period = "2016Q2/", options = list(report = "none"))

  data_expected <- mdl_solved$get_data()
  data_expected["2016Q1", "c"] <- 999
  expect_equal(
    mdl2$get_data(),
    data_expected
  )

  mdl2 <- mdl$copy()
  mdl2$set_values(999, names = "c", period = "2015Q2/2016Q1")
  mdl2$solve(period = "/2015Q4", options = list(report = "none"))

  data_expected <- mdl_solved$get_data()
  data_expected["2016Q1", "c"] <- 999
  expect_equal(
    mdl2$get_data(),
    data_expected
  )
})

test_that("errors", {
  mdl2 <- mdl$copy()
  expect_error(
    mdl2$solve("2016"),
    paste("The specified period (2016Q1/2016Q4) is not compatible with the data",
          "period (2015Q1/2016Q3). The period should lie within the range",
          "2015Q2/2016Q3."),
    fixed = TRUE
  )
  expect_true(isTRUE(all.equal(mdl, mdl2)))
})
