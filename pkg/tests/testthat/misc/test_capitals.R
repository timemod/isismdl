library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE

context("mrf")

source("../tools/read_mrf.R")

Sys.setlocale("LC_COLLATE", "C")

mdl_file <- "mdl/capitals.mdl"
mdl <- isis_mdl(mdl_file, silent = TRUE, period = 2021)
mdl$set_values(0.5, names = "A", period = 2020)
data <- regts(matrix(0.5, ncol = 1), period = 2020, names = "Aa")
mdl$set_data(data)

test_that("solve" , {
  expect_silent(mdl$solve(options = list(report = "none")))
  expect_equal(mdl$get_solve_status(), "OK")

  data <<- mdl$get_data()
  expect_known_value(data, file = "expected_output/capitals_data.rds",
                     update = update)
})

test_that("get_data with specific names", {
  expect_equal(mdl$get_data(names = "a"), data[, "a", drop = FALSE])
  expect_equal(mdl$get_data(names = "A"), data[, "A", drop = FALSE])
  expect_equal(mdl$get_data(names = "Aa"), data[, "Aa", drop = FALSE])
  expect_equal(mdl$get_data(names = "aa"), data[, "aa", drop = FALSE])
})

test_that("mrf", {
  mrf_data <- read_mrf(mdl_file)
  expect_known_output(cat(mrf_data), file = "expected_output/capitals_mrf.txt",
                      update = update, print = TRUE)
})
