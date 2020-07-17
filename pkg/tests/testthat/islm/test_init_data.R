library(utils)
library(isismdl)
library(testthat)

context("init_data for the ISLM model")

rm(list = ls())

rep <- capture.output(mdl <- islm_mdl())

test_that("init_data gives an error if the data period has not been set", {
  msg <- paste("If neither data_period nor data have been specified,",
               "then the data period\nshould have been set before",
               "with method init_data or set_period.")
  expect_error(mdl$init_data(), msg)
})

test_that("init_data does not change the model period", {
  mp <- period_range("2019", 2019)
  mdl$set_period(mp)
  mdp <- period_range(2017, 2022)
  mdl$init_data(mdp)
  expect_equal(mdl$get_period(), mp)
  expect_equal(mdl$get_data_period(), mdp)

  mdl$init_data()
  expect_equal(mdl$get_period(), mp)
  expect_equal(mdl$get_data_period(), mdp)
  expect_true(all(is.na(mdl$get_data())))
  expect_true(all(mdl$get_ca() == 0))
})

test_that("data period outside range required by solve", {
  msg <- "The data period should include the range 2018/2019\\."
  expect_error(mdl$init_data(data_period = "2030/2034"), msg)
  expect_error(mdl$init_data(data_period = "2019"), msg)
})


