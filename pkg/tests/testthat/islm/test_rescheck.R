library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

context("run_eqn for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))

mdl$set_solve_options(mode = "reschk", report = "none")

test_that("residual check for solved model", {
  mdl2 <- mdl$copy()
  mdl2$solve()
  expect_equal(mdl$get_data(), mdl2$get_data())
  expect_equal(mdl$get_ca(), mdl2$get_ca())
})

test_that("residual check in combination with fixing", {
  mdl2 <- mdl$copy()
  fix_per <- "2015Q4/2016Q1"
  mdl2$set_fix_values(600, names = "c", period = fix_per)

  # restore old data (so that y won't change) and perfrom a residual check
  mdl2$set_data(mdl$get_data(names = "c"))
  mdl2$solve()

  expected_data <- mdl$get_data()
  expected_data[fix_per, "c"] <- 600
  expect_equal(mdl2$get_data(), expected_data)

  expected_ca <- mdl$get_ca()
  expected_ca[fix_per, "c"] <- 600 - mdl$get_data(names = "c", period = fix_per)
  expect_equal(mdl2$get_ca(), expected_ca)
})
