library(isismdl)
library(testthat)
library(readr)

rm(list = ls())

update_expected <- FALSE


test_that("error given", {
  msg <- "Error detected in compilation of model mdl/params1\nCheck the .err file"
  expect_error(capture_output(mdl <- isis_mdl("mdl/params1")), msg)
})

test_that("error file correct", {
  error_txt <- read_file("mdl/params1.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  #cat(error_txt)
  expect_known_output(cat(error_txt), "expected_output/params1.txt",
                      update = update_expected)
})
