# Syntax errors in if expressions

library(isismdl)
library(testthat)
library(readr)

rm(list = ls())

update_expected <- FALSE

source("../tools/expect_known_error_file.R")

test_that("error given", {
  msg <- "Error detected in compilation of model mdl/if1\nCheck the .err file"
  expect_error(
    isis_mdl("mdl/if1", silent = TRUE),
    msg,
    fixed = TRUE
  )
})

test_that("error file correct", {
  expect_known_error_file(
    "mdl/if1.err",
    file = "expected_output/if1.err",
    update = update_expected
  )
})
