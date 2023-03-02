library(utils)
library(isismdl)
library(testthat)

rm(list = ls())


mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

new_ca <- mdl$get_ca()
new_ca[ , "c" ] <- 10
new_ca["2015Q3/2016Q2", c("t", "i")] <- 1:4
new_ca["2016Q2", "t"] <- 8

test_that("set_values works correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_ca_values(10, names = "c")
  mdl2$set_ca_values(1:4, names = c("t", "i"), period = "2015Q3/2016Q2")
  mdl2$set_ca_values(8, pattern = "^t$", period = "2016Q2")
  expect_equal(mdl2$get_ca(), new_ca)
})

test_that("set_ca_values handles errors correctly", {
  mdl2 <- mdl$copy()
  msg <- "\"y\" is not a frml variable"
  expect_error(mdl2$set_ca_values(1, names = "y"), msg)
  msg <- "The following names are no frml variables: \"y\", \"xxx\"."
  expect_error(mdl2$set_ca_values(1, names = c("y", "xxx", "c")), msg)
})
