library(utils)
library(isismdl)
library(testthat)

rm(list = ls())


mdl <- read_mdl("islm_model.ismdl", silent = TRUE)

names <- mdl$get_endo_names(type = "frml")
nperiod <- nperiod(mdl$get_data_period())
set.seed(123)
ca <- matrix(rnorm(nperiod * length(names)), nrow = nperiod)
labels <- paste(mdl$get_labels()[names], "(constant adjustment)")
ca <- regts(ca, period = mdl$get_data_period(), names = names, labels = labels)
mdl$set_ca(ca)

test_that("get_ca works correctly", {
  expect_equal(mdl$get_ca(), ca)
  expect_equal(mdl$get_ca(period = "2016Q1/"), ca["2016Q1/2016Q3", ])
  expect_equal(mdl$get_ca(period = "/2016Q1", names = c("c", "i")),
               ca["2015Q1/2016Q1", c("c", "i")])
  expect_equal(mdl$get_ca(period = "2016Q1/2016Q3", pattern = "^t"),
               ca["2016Q1/2016Q3", "t", drop = FALSE])
  expect_equal(mdl$get_ca(period = "2015"), ca["2015Q1/2015Q4", ])
  expect_equal(mdl$get_ca(period = "2016M1/2016M2"), ca["2016Q1", ])
})

test_that("get_ca handles errors correctly", {
  msg <- "'xxx' is not a frml variable"
  expect_error(mdl$get_ca(names = "xxx"), msg)
  msg <- "'y' is not a frml variable"
  expect_error(mdl$get_ca(names = c("c", "y")), msg)
  msg <- "'y' is not a frml variable"
  expect_error(mdl$get_ca(names = "y", pattern = "^y"), msg)
  msg <- "The following names are not frml variables: 'xxx' and 'y'."
  expect_error(mdl$get_ca(names = c("xxx", "y")), msg)
})
