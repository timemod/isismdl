library(utils)
library(isismdl)
library(testthat)

context("get_data for the  ISLM model")

capture_output(mdl <- read_mdl("islm_model.rds"))

names <- mdl$get_var_names()
nperiod <- nperiod(mdl$get_data_period())
set.seed(123)
data <- matrix(rnorm(nperiod * length(names)), nrow = nperiod)
data <- regts(data, period = mdl$get_data_period(), names = names,
              labels = mdl$get_labels())
mdl$set_data(data)

test_that("get_data works correctly", {
  expect_equal(mdl$get_data(), data)
  expect_equal(mdl$get_data(period = "2016Q1/"), data["2016Q1/2016Q3", ])
  expect_equal(mdl$get_data(period = "/2016Q1", names = c("c", "g")),
               data["2015Q1/2016Q1", c("c", "g")])
  expect_equal(mdl$get_data(period = "2016Q1/2016Q3", pattern = "^y"),
               data["2016Q1/2016Q3", c("y", "yd")])
  expect_equal(mdl$get_data(period = "2016Q1/2016Q3", pattern = "^y",
                            names = c("y")),
               data["2016Q1/2016Q3", c("y", "yd")])

  expect_equal(mdl$get_data(period = "2016"), data["2016Q1/2016Q4", ])
  expect_equal(mdl$get_data(period = "2015M6"), data["2015Q2", ])
})

test_that("get_data handles errors correctly", {
  msg <- "xxx is not a model variable"
  expect_error(mdl$get_data(names = "xxx"), msg)
  msg <- "xxx is not a model variable"
  expect_error(mdl$get_data(names = c("c", "xxx")), msg)
  msg <- "xxx is not a model variable"
  expect_error(mdl$get_data(names = "xxx", pattern = "^y"), msg)
  msg <-"The variables xxx yyy are no model variables"
  expect_error(mdl$get_data(names = c("xxx", "yyy")), msg)
})
