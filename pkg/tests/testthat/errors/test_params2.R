library(isismdl)
library(testthat)
library(readr)

context("syntax error params (2)")

test_that("error given", {
  msg <- "Error detected in compilation of model mdl/params2\nCheck the .err file"
  expect_error(capture_output(mdl <- isis_mdl("mdl/params2")), msg)
})

test_that("error file correct", {
  error_txt <- read_file("mdl/params2.err")
  #cat(error_txt)
  expect_equal_to_reference(error_txt, "expected_output/params2.rds")
})
