library(isismdl)
library(testthat)
library(readr)

update_expected <- FALSE

context("syntax error user function")

test_that("error given", {
  msg <- "Error detected in compilation of model mdl/ufunc1\nCheck the .err file"
  expect_error(capture_output(mdl <- isis_mdl("mdl/ufunc1")), msg)
})

test_that("error file correct", {
  error_txt <- read_file("mdl/ufunc1.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  #cat(error_txt)
  expect_known_output(cat(error_txt), "expected_output/ulfunc1.txt",
                      update = update_expected)
})

