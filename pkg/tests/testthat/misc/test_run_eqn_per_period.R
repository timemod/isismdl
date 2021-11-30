library(isismdl)
library(testthat)

rm(list = ls())

context("run_eqn per period")

mdl_file <- "mdl/test_run_eqn_per_period.mdl"

solve_per <- period_range(2021, 2023)
mdl <- isis_mdl(mdl_file, silent = TRUE, period = solve_per)

mdp <- period_range(2021, 2024)

test_that("per_period FALSE", {
  mdl1 <- mdl$copy()
  mdl1$run_eqn(names = "x2")
  mdl1$run_eqn(names = c("x0", "x1"),
               per_period = FALSE, forwards = FALSE,  period = solve_per)
  expected_result <- cbind(x0 = NA,
                           x1 = regts(c(2, 2,  2, NA), period = mdp),
                           x2 = 1, x4 = NA)
  expect_equal(mdl1$get_data(), expected_result)
})

test_that("period_period TRUE", {
  mdl1 <- mdl$copy()
  mdl1$run_eqn(names = "x2")
  mdl1$run_eqn(names = c("x0", "x1"),
               per_period = TRUE, forwards = FALSE,  period = solve_per)
  expected_result <- cbind(x0 = regts(c(3, 3, NA, NA), period  = mdp),
                           x1 = regts(c(2, 2,  2, NA), period = mdp),
                           x2 = 1, x4 = NA)
  expect_equal(mdl1$get_data(), expected_result)

  expect_equal(mdl1$get_ca(),
               cbind(x1 = regts(0, period = mdp)))
})

test_that("all equations", {
  mdl1 <- mdl$copy()
  mdl1$run_eqn(per_period = TRUE, forwards = FALSE)
  expected_result <- cbind(x0 = regts(c(3, 3, NA, NA), period  = mdp),
                           x1 = regts(c(2, 2,  2, NA), period = mdp),
                           x2 = 1, x4 = regts(999, period = mdp))
  expect_equal(mdl1$get_data(), expected_result)
})

test_that("period_period TRUE, forwards = TRUE", {
  mdl1 <- mdl$copy()
  mdl1$run_eqn(names = "x2")
  mdl1$run_eqn(names = c("x0", "x1"),
               per_period = TRUE, forwards = TRUE,  period = solve_per)
  expected_result <- cbind(x0 = NA,
                           x1 = regts(c(2, 2,  2, NA), period = mdp),
                           x2 = 1, x4 = NA)
  expect_equal(mdl1$get_data(), expected_result)
})

test_that("error", {
  msg <- "Argument 'per_period' should be a TRUE or FALSE"
  expect_error(mdl$run_eqn(per_period = 2), msg)
  expect_error(mdl$run_eqn(per_period = NA), msg)
  expect_error(mdl$run_eqn(per_period = c(TRUE, TRUE)), msg)
})


test_that("inactive equation", {
  mdl1 <- mdl$copy()
  mdl1$set_eq_status(names = "x1", status = "inactive")
  expect_error(
    mdl1$run_eqn(names = c("x2", "x0", "x1"),
               per_period = TRUE, forwards = FALSE),
    "\"x1\" is not an active equation.", fixed = TRUE)

  mdl1$run_eqn(per_period = TRUE, forwards = FALSE)
  expected_result <- cbind(x0 = regts(NA_real_, period = mdp),
                           x1 = NA,
                           x2 = 1, x4 = 999)
  expect_equal(mdl1$get_data(), expected_result)
})

test_that("fixed equation", {
  mdl1 <- mdl$copy()
  mdl1$set_fix_values(10, names = "x1", period = 2022)
  mdl1$run_eqn(per_period = TRUE, forwards = FALSE)
  expected_result <- cbind(x0 = regts(c(11, 3, NA, NA), period  = mdp),
                           x1 = regts(c(2, 10,  2, NA), period = mdp),
                           x2 = 1, x4 = regts(999, period = mdp))
  expect_equal(mdl1$get_data(), expected_result)

  expect_equal(mdl1$get_ca(),
               cbind(x1 = regts(c(0, 8, 0, 0), period = mdp)))
})


