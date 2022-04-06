library(isismdl)
library(testthat)
library(readr)

rm(list = ls())

update_expected <- FALSE

context("syntax error nesting (2)")

test_that("error given", {
  msg <- "Error detected in compilation of model mdl/nesting2\nCheck the .err file"
  expect_error(capture_output(mdl <- isis_mdl("mdl/nesting2")), msg)
})

test_that("error file correct", {
  error_txt <- read_file("mdl/nesting2.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  #cat(error_txt)
  expect_known_output(cat(error_txt), "expected_output/nesting2.txt",
                      update = update_expected)
})
