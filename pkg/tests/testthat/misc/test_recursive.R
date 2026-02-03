library(utils)
library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE


source("../tools/read_mrf.R")

period <- as.period_range("1550Y")

mdl_filename <- "mdl/recursive.mdl"
mdl <- isis_mdl(mdl_filename, period, silent = TRUE)
mdl$set_solve_options(report = "none", maxiter = 0)
mdl_solved <- mdl$copy()
mdl_solved$solve()

names <- mdl$get_var_names()
expected_result <- regts(matrix(1, nrow = 1, ncol = length(names)),
                        period = period, names = names)

test_that("run_eqn", {
  mdl2 <- mdl$copy()
  mdl2$run_eqn()
  expect_equal(mdl2$get_data(), expected_result)
})

test_that("run_eqn with natural ordering", {
  mdl2 <- mdl$copy()
  mdl2$run_eqn(names = mdl$get_eq_names(order = "natural"))
  expected_result_err <- regts(matrix(c(1, 1, NA, NA), nrow = 1, ncol = length(names)),
                               period = period, names = names)
  expect_equal(mdl2$get_data(), expected_result_err)
})

test_that("run_eqn with alphabetical ordering", {
  # in this case, alphabetic order is gives same result as solve order
  mdl2 <- mdl$copy()
  mdl2$run_eqn(names = mdl$get_eq_names(order = "sorted"))
  expect_equal(mdl2$get_data(), expected_result)
})

test_that("run_eqn with solve ordering", {
  mdl2 <- mdl$copy()
  mdl2$run_eqn(names = mdl$get_eq_names(order = "solve"))
  expect_equal(mdl2$get_data(), expected_result)
  mdl2$solve(options = list(maxiter = 0))
  expect_equal(mdl2, mdl_solved)
})

test_that("solve", {
  mdl2 <- mdl$copy()
  mdl2$solve()
  expect_equal(mdl2$get_data(), expected_result)
})

test_that("fill_mdl_data", {
  mdl2 <- mdl$copy()
  mdl2$fill_mdl_data(report = "no")
  expect_equal(mdl2$get_data(), expected_result)
})

test_that("check mrf", {
  mrf_data <- read_mrf(mdl_filename)
  expect_known_output(cat(mrf_data),
                      file = "expected_output/recursive_mrf.txt",
                      update = update, print = TRUE)

})
