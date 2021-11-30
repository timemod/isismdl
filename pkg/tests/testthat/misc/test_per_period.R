library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE

context("per period")

mdl_file <- "mdl/test_per_period.mdl"

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


