library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

context("equation debugging for the ISLM model")

source("../tools/convert_report.R")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))

test_that("set/get debug_eqn", {
  expect_false(mdl$get_debug_eqn())
  mdl$set_debug_eqn(FALSE)
  expect_false(mdl$get_debug_eqn())
  mdl$set_debug_eqn(TRUE)
  expect_true(mdl$get_debug_eqn())
})

test_that("comparing models", {
  mdl2 <- mdl$copy()
  mdl2$set_debug_eqn(FALSE)
  expect_true(mdl$get_debug_eqn())
  expect_false(isTRUE(all.equal(mdl, mdl2)))
})

test_that("reading/writing model", {
  mdl$write_mdl("temp.ismdl")
  dum <- capture.output(mdl2 <- read_mdl("temp.ismdl"))
  unlink("temp.ismdl")
  expect_true(mdl2$get_debug_eqn())
  expect_equal(mdl, mdl2)
})

test_that("debug statements for solve", {
  mdl2 <- mdl$copy()
  mdl2$set_values(0, names = "ms", period = "2015Q2/2015Q3")
  old_data <- mdl2$get_data()
  report <- capture.output({
    suppressWarnings(mdl2$solve())
  })
  expected_data <- old_data
  expected_data["2015Q2", "r"] <- NA
  expect_equal(mdl2$get_data(), expected_data)

  #cat(paste(report, collapse = "\n"))
  expected_report_file <- "expected_output/debug_report1.txt"
  expect_known_output(cat_report(convert_report(report)), expected_report_file)
})

test_that("debug statements for run_eqn", {
  mdl2 <- mdl$copy()
  mdl2$set_values(0, names = "ms", period = "2015Q2/2015Q3")
  old_data <- mdl2$get_data()
  report <- capture.output({
    mdl2$run_eqn(period = mdl$get_period())
  })
  expected_data <- old_data
  expected_data["2015Q2/2015Q3", "r"] <- NA
  expect_equal(mdl2$get_data(), expected_data)

  #cat(paste(report, collapse = "\n"))
  expected_report_file <- "expected_output/debug_report2.txt"
  expect_known_output(cat_report(convert_report(report)), expected_report_file)
})
