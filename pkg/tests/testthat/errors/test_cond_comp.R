# Syntax errors for conditional compilation

library(isismdl)
library(testthat)
library(readr)

rm(list = ls())

update_expected <- FALSE

source("../tools/expect_known_error_file.R")

compile_mdl <- function(model_text, expected_emsg, ...) {
  mdl_file <- tempfile("isismdl_test_", fileext = ".mdl")
  err_file <- sub("\\.mdl$", ".err", mdl_file)
  writeLines(model_text, mdl_file)
  expect_error(
    isis_mdl(mdl_file, silent = TRUE, ...),
    expected_emsg
  )
  if (file.exists(err_file)) {
    lines <- readLines(err_file)
    lines <- sub(mdl_file, "model_text", lines)
    return(paste(lines, collapse = "\n"))
  } else {
    return(NULL)
  }
}

test_that("missing endif", {
  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  model_text <- glue::glue("
     x = 1;
     #if a
       y = 2;
       #if b
       z = 2;
     #endif
     l = 2;
    ")

  err_text <- compile_mdl(model_text, msg)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_1.err",
    update = update_expected
  )

  # with compiler flag "a" the result must be the same
  err_text_2 <- compile_mdl(model_text, msg, parse_options = list(flags = "a"))

  expect_equal(err_text, err_text_2)
})
