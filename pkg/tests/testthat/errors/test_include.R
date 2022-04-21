library(isismdl)
library(testthat)
library(readr)

rm(list = ls())
context("end statement in include")

update_expected <- FALSE

test_that("include1", {
  msg <- "Error detected in compilation of model mdl/include1\nCheck the .err file"
  expect_error(isis_mdl("mdl/include1", silent = TRUE), msg)

  error_txt <- read_file("mdl/include1.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  expect_known_output(cat(error_txt), "expected_output/include1.txt",
                      update = update_expected)
})

test_that("include2", {
  msg <- "Error detected in compilation of model mdl/include2\nCheck the .err file"
  expect_error(isis_mdl("mdl/include2", silent = TRUE), msg)

  error_txt <- read_file("mdl/include2.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  expect_known_output(cat(error_txt), "expected_output/include2_a.txt",
                      update = update_expected)

  # now with argument include dir
  expect_error(isis_mdl("mdl/include2", silent = TRUE,
                        parse_options = list(include_dirs = "mdl/incl")), msg)

  error_txt <- read_file("mdl/include2.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  expect_known_output(cat(error_txt), "expected_output/include2_b.txt",
                      update = update_expected)
})

test_that("include3", {
  # TODO: there is no error about line 7 in the mdl/include3.mdl.
  # Why not?
  msg <- "Error detected in compilation of model mdl/include3\nCheck the .err file"
  expect_error(isis_mdl("mdl/include3", silent = TRUE), msg)

  error_txt <- read_file("mdl/include3.err")
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }
  expect_known_output(cat(error_txt), "expected_output/include3.txt",
                      update = update_expected)
})


