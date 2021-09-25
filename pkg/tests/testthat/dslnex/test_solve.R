library(utils)
library(testthat)
library(isismdl)

rm(list = ls())
update <- FALSE

context("solve dslnex model")

source("../tools/read_mrf.R")

mdl_file <- "mdl/dslnex.mdl"

input_data <- regts(matrix(c(2, 0.5), ncol = 2), names = c("x1", "x2"))
correct_result <- regts(matrix(c(1, 1), ncol = 2), names = c("x1", "x2"))

model <- isis_mdl(model_file = mdl_file, data = input_data, silent = TRUE)

model$set_solve_options(report = "none")

test_that("the result of solve is correct", {
  expect_silent(model$solve())
  expect_equal(model$get_data(names = c("x1", "x2")), correct_result)
})

test_that("model also solved with small rlxmax", {
  model$set_data(input_data)
  expect_equal(model$get_data(names = c("x1", "x2")), input_data)

  msg <- "Simulation stopped"
  # with small maxiter the model will not solve
  expect_warning(model$solve(options = list(rlxmax = 0.05, maxiter = 50,
                                            report = "none")), msg)

  expect_silent(model$solve(options = list(rlxmax = 0.05, maxiter = 500,
                                            report = "none")))
  expect_equal(model$get_data(names = c("x1", "x2")), correct_result)

  msg <- "rlxmin is smaller than rlxmax"
  expect_error(model$solve(options = list(rlxmax = 0.05, rlxmin = 0.08,
                                           report = "none")), msg)
})

test_that("check mrf", {
  mrf_data <- read_mrf(mdl_file)
  expect_known_output(cat(mrf_data),
                      file = "expected_output/solve_mrf.txt",
                      update = update, print = TRUE)
})



