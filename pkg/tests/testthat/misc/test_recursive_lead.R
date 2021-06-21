library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

context("test for a simple recursive model with leads")

period <- as.period_range("1550Y/1551y")

mdl <- isis_mdl("mdl/recursive_lead.mdl", period, silent = TRUE)
mdl$set_values(names = "a", value = 0)

test_that("solving", {
  expect_output(mdl$solve(),
              "Convergence for consistent expectations after   2 rounds")
  expect_equal(mdl$get_solve_status(), "OK")
})

test_that("result", {
  period <- period_range(1550, 1552)
  a <- regts(c(1, 1, 0), period = period)
  x <- regts(c(1, 0, NA), period = period)
  y <- x
  z <- x
  expected_result <- cbind(a, x, y, z)
  expect_equal(mdl$get_data(), expected_result)
})
