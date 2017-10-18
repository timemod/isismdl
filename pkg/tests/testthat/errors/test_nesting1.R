library(isismdl)
library(testthat)
library(readr)

context("syntax error nesting (1)")

test_that("error given", {
  msg <- "Error detected in compilation of model mdl/nesting1\nCheck the .err file"
  expect_error(capture_output(mdl <- isis_mdl("mdl/nesting1")), msg)
})

test_that("error file correct", {
  error_txt <- read_file("mdl/nesting1.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  #cat(error_txt)
  expect_equal_to_reference(error_txt, "expected_output/nesting1.rds")
})
