library(isismdl)
library(testthat)
library(readr)

context("syntax error params (1)")

test_that("error given", {
  msg <- "Error detected in compilation of model mdl/params1\nCheck the .err file"
  expect_error(capture_output(mdl <- isis_mdl("mdl/params1")), msg)
})

test_that("error file correct", {
  error_txt <- read_file("mdl/params1.err")
  #cat(error_txt)
  expect_equal_to_reference(error_txt, "expected_output/params1.rds")
})
