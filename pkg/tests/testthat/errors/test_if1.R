library(isismdl)
library(testthat)
library(readr)

context("syntax error if statement")

test_that("error given", {
  msg <- "Error detected in compilation of model mdl/if1\nCheck the .err file"
  expect_error(capture_output(mdl <- isis_mdl("mdl/if1")), msg)
})

test_that("error file correct", {
  error_txt <- read_file("mdl/if1.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  #cat(error_txt)
  expect_known_output(cat(error_txt), "expected_output/if1.txt")
})
