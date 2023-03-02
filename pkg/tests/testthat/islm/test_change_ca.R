library(utils)
library(isismdl)
library(testthat)

rm(list = ls())


mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

test_that("change_ca works correctly (1)", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$change_ca(function(x) {x + 10}, names = "c")
  mdl2$change_ca(function(x, dx) {x + dx}, names = c("t", "i"),
                 period = "2015Q3/2016Q2", dx = 1:4)
  mdl2$change_ca(function(x) {sin(x) +8}, pattern = "^t$", period = "2016Q2")

  expected <- mdl$get_ca()
  expected[ , "c" ] <- 10
  expected["2015Q3/2016Q2", c("t", "i")] <- 1:4
  expected["2016Q2", "t"] <- sin(expected["2016Q2", "t"]) + 8

  expect_equal(mdl2$get_ca(), expected)
})

test_that("change_ca works correctly (2)", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$change_ca(function(x) {x + 10}, names = "c", period = "2015")
  mdl2$change_ca(function(x, dx) {x + dx}, names = c("t", "i"),
                 period = "2016M7", dx = 1:1)
  mdl2$change_ca(function(x) {sin(x) + 8}, pattern = "^t$", period = "/2016M3")

  expected <- mdl$get_ca()
  expected["2015Q1/2015Q4" , "c" ] <- 10
  expected["2016Q3", c("t", "i")] <- 1:1
  expected["2015Q1/2016Q1", "t"] <- sin(expected["2015Q1/2016Q1", "t"]) + 8

  expect_equal(mdl2$get_ca(), expected)
})


test_that("change_ca handles errors correctly", {
  f <- function(x) {x}
  mdl2 <- mdl$clone(deep = TRUE)
  msg <- "\"y\" is not a frml variable"
  expect_error(mdl2$change_ca(f, names = "y"), msg)
  msg <- "The following names are no frml variables: \"y\", \"xxx\"."
  expect_error(mdl2$change_ca(f, names = c("y", "xxx", "c")), msg)
})
