library(utils)
library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE


source("../tools/read_mrf.R")

period <- as.period_range("1550Y/1551y")

mdl_filename <- "mdl/recursive_lead.mdl"
mdl <- isis_mdl(mdl_filename = mdl_filename, period = period, silent = TRUE)
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


test_that("check mrf", {
  mrf_data <- read_mrf(mdl_filename)
  expect_known_output(cat(mrf_data),
                      file = "expected_output/recursive_lead_mrf.txt",
                      update = update, print = TRUE)

})
