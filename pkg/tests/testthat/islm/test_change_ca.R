library(utils)
library(isismdl)
library(testthat)

context("set_values for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

new_ca <- mdl$get_ca()
new_ca[ , "c" ] <- 10
new_ca["2015Q3/2016Q2", c("t", "i")] <- 1:4
new_ca["2016Q2", "t"] <- sin(new_ca["2016Q2", "t"]) + 8

test_that("set_values works correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$change_ca(function(x) {x + 10}, names = "c")
  mdl2$change_ca(function(x, dx) {x + dx}, names = c("t", "i"),
                 period = "2015Q3/2016Q2", dx = 1:4)
  mdl2$change_ca(function(x) {sin(x) +8}, pattern = "^t$", period = "2016Q2")
  expect_equal(mdl2$get_ca(), new_ca)
})

test_that("set_ca_values handles errors correctly", {
  f <- function(x) {x}
  mdl2 <- mdl$clone(deep = TRUE)
  msg <- "y is not a stochastic model variable"
  expect_error(mdl2$change_ca(f, names = "y"), msg)
  msg <- "The variables y xxx are no stochastic model variables"
  expect_error(mdl2$change_ca(f, names = c("y", "xxx", "c")), msg)
})
