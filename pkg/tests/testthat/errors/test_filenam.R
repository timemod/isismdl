library(isismdl)
library(testthat)
library(readr)

context("user language functions (1)")

rm(list = ls())

mdl_file <- "mdl/singular1.mdl"

test_that("file does not exist", {
  expect_error(isis_mdl(2),
               "Argument 'model_file' must be a character vector of length 1")
  expect_error(isis_mdl(rep(mdl_file, 2)),
               "Argument 'model_file' must be a character vector of length 1")
  expect_error(isis_mdl("aap.mdl", silent = TRUE),
               "The model file aap.mdl does not exist\n")
  expect_error(isis_mdl("mdl", silent = TRUE),
               "'mdl' is a directory")

  if (.Platform$OS.type == "unix") {
    mdl_file_abs <- normalizePath(mdl_file)
    mdl_file_tilde <- sub(Sys.getenv("HOME"), "~", mdl_file_abs)
    expect_silent(isis_mdl(mdl_file_tilde, silent = TRUE))
  }
})
