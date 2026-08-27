# Syntax errors for conditional compilation

library(isismdl)
library(testthat)
library(readr)

rm(list = ls())

update_expected <- FALSE

source("../tools/expect_known_error_file.R")
source("tools/get_error_text.R")


test_that("syntax errors for model with long line", {

  n <- 400
  model_lines <- c(
    paste0("x = ", paste(1:n, collapse = " + "), "***4;"),
    "z = 2 *** 3;"
  )

  model_text <- paste(model_lines, collapse = "\n")

  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"
  err_text <- get_error_text(model_text, msg)

  expect_known_output(
    cat(err_text),
    file = "expected_output/long_line_1.err",
    update = update_expected
  )
})
