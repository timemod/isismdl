library(utils)
library(isismdl)
library(testthat)

context("get_last_solve_period")

test_that("new compiled moel", {
  capture.output(mdl <- isis_mdl("mdl/islm.mdl"))
  expect_null(mdl$get_last_solve_period())
})

rep <- capture.output(mdl <- read_mdl("islm_model.ismdl"))

rds_file <- tempfile()

test_that("solved model", {
  expect_null(mdl$get_last_solve_period())
  mdl$solve(options = list(report = "none"))
  expect_equal(mdl$get_last_solve_period(), period("2016q3"))
  mdl2 <- mdl$copy()
  mdl$set_values(NA, names = "g", period = "2016q1")
  expect_warning(mdl$solve(options = list(report = "none")))
  expect_equal(mdl$get_last_solve_period(), period("2016q1"))
  mdl$write_mdl(rds_file)
  expect_equal(mdl2$get_last_solve_period(), period("2016q3"))
})

test_that("read model", {
  capture.output(mdl2 <- read_mdl(rds_file))
  expect_equal(mdl2$get_last_solve_period(), period("2016q1"))
  mdl2$set_period("2015q4/2016q1")
  expect_equal(mdl2$get_last_solve_period(), period("2016q1"))
  mdl2$set_period("2015q4")
  expect_null(mdl2$get_last_solve_period())
  capture.output(mdl2 <- read_mdl(rds_file))
  mdl2$set_period("2016q2")
  expect_null(mdl2$get_last_solve_period())
})
