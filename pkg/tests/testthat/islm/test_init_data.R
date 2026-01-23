library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

invisible(capture.output(mdl <- islm_mdl()))

test_that("init_data gives an error if the data period has not been set", {
  msg <- paste("If neither data_period nor data have been specified,",
               "then the data period\nshould have been set before",
               "with method init_data or set_period.")
  expect_error(mdl$copy()$init_data(), msg)
})

mdl2 <- mdl$copy()

test_that("init_data does not change the model period", {
  mp <- period_range("2019", 2019)
  mdl2$set_period(mp)
  mdp <- period_range(2017, 2022)
  mdl2$init_data(mdp)
  expect_equal(mdl2$get_period(), mp)
  expect_equal(mdl2$get_data_period(), mdp)

  mdl2$init_data()
  expect_equal(mdl2$get_period(), mp)
  expect_equal(mdl2$get_data_period(), mdp)
  expect_true(all(is.na(mdl2$get_data())))
  expect_true(all(mdl2$get_ca() == 0))
})

test_that("data period outside range required by solve", {
  msg <- "The data period should include the range 2018/2019\\."
  expect_error(mdl2$init_data(data_period = "2030/2034"), msg)
  expect_error(mdl2$init_data(data_period = "2019"), msg)
})

test_that("only data specified", {

  data <- cbind(c= regts(1:6, period = "2011q1/2012q1"))
  mdl2 <- mdl$copy()
  mdl2$init_data(data = data)
  expect_equal(mdl2$get_period(), period_range("2011q2/2012q1"))
  expect_equal(mdl2$get_data_period(), period_range("2011q1/2012q1"))

  # errors
  mdl2 <- mdl$copy()

  mdl2$set_period("2011m1/2012m3")

  expect_error(
    mdl2$init_data(data = data),
    "The data has a different frequency (4) than the model period (12).",
    fixed = TRUE
  )
})

test_that("data_period and data not specified", {
  mdl2 <- mdl$copy()
  mdl2$set_period("2011q1/2012q1")
  data_empty <- mdl2$get_data()
  mdl2$set_values(999, names = "c")
  mdl2$init_data()
  expect_equal(mdl2$get_data(), data_empty)

  # errors
  mdl2 <- mdl$copy()
  expect_error(
    mdl2$init_data(),
    paste("If neither data_period nor data have been specified, then the data",
          "period\nshould have been set before with method init_data or set_period."),
    fixed = TRUE
  )
})
