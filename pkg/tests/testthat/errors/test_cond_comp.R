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

test_that("duplicate else", {
  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  model_text <- glue::glue("
     x = 1;
     #if a
       y = 2;
     #else
       z = 3;
     #else
       w = 4;
     #endif
    ")

  err_text <- compile_mdl(model_text, msg)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_2.err",
    update = update_expected
  )
})

test_that("stray else without matching if", {
  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  model_text <- glue::glue("
     x = 1;
     #else
       y = 2;
    ")

  err_text <- compile_mdl(model_text, msg)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_3.err",
    update = update_expected
  )
})

test_that("stray elseif without matching if", {
  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  model_text <- glue::glue("
     x = 1;
     #elseif a
       y = 2;
    ")

  err_text <- compile_mdl(model_text, msg)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_4.err",
    update = update_expected
  )
})

test_that("stray endif without matching if", {
  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  model_text <- glue::glue("
     x = 1;
     #endif
    ")

  err_text <- compile_mdl(model_text, msg)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_5.err",
    update = update_expected
  )
})

test_that("missing endif with deep nesting (3 levels)", {
  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  model_text <- glue::glue("
     x = 1;
     #if a
       y = 2;
       #if b
         z = 3;
         #if c
           w = 4;
         #endif
       l = 5;
    ")

  err_text <- compile_mdl(model_text, msg)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_6.err",
    update = update_expected
  )
})

test_that("maximum #if nesting level (10)", {

  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  model_text <- glue::glue("
#if a1
#if a2
#if a3
#if a4
#if a5
#if a6
#if a7
#if a8
#if a9
#if a10
#if a11
x = 1;
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
")



  err_text <- compile_mdl(
    model_text, msg,
    parse_options = list(flags = paste0("a", 1:11, collapse = ",")))

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_7.err",
    update = TRUE
  )
})

test_that("#IF in included filename", {

  expect_error(
    mdl <- isis_mdl("mdl/cond_comp1.mdl", silent = TRUE)
  )

  update_expected <- TRUE
  expect_known_error_file(
    "mdl/cond_comp1.err",
    file = "expected_output/cond_comp_error_8.err",
    update = update_expected
  )

  expect_error(
    mdl <- isis_mdl("mdl/cond_comp1.mdl",
                    parse_options = list(flags = "incl"),
                    silent = TRUE)
  )
  expect_known_error_file(
    "mdl/cond_comp1.err",
    file = "expected_output/cond_comp_error_9.err",
    update = update_expected
  )
})
