library(isismdl)
library(testthat)
library(readr)

context("syntax error varia")

test_that("error given", {
  msg <- "Error detected in compilation of model mdl/varia1\nCheck the .err file"
  expect_error(mdl <- isis_mdl("mdl/varia1", silent = TRUE), msg)
})

test_that("error file correct", {
  error_txt <- read_file("mdl/varia1.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  expect_known_output(cat(error_txt), "expected_output/varia1.err")
})

