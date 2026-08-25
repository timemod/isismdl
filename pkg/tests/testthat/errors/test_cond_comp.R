# Syntax errors for conditional compilation

library(isismdl)
library(testthat)
library(readr)

rm(list = ls())

update_expected <- FALSE

source("../tools/expect_known_error_file.R")
source("tools/get_error_text.R")

test_that("missing endif", {

  model_text <- glue::glue("
     x = 1;
     #if a
       y = 2;
       #if b
       z = 2;
     #endif
     l = 2;
    ")

  err_text <- get_error_text(model_text)
  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_1.err",
    update = update_expected
  )

  # with compiler flag "a" the result must be the same
  err_text_2 <- get_error_text(model_text, flags = "a")
  expect_equal(err_text, err_text_2)

  err_text_3 <- get_error_text(model_text, flags = c("a", "b"))
  expect_equal(err_text, err_text_3)

})

test_that("duplicate else", {

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

  err_text <- get_error_text(model_text)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_2.err",
    update = update_expected
  )

  err_text_2 <- get_error_text(model_text, flags = "a")
  expect_equal(err_text, err_text_2)
})

test_that("stray else without matching if", {
  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  model_text <- glue::glue("
     x = 1;
     #else
       y = 2;
    ")

  err_text <- get_error_text(model_text, msg)

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

  err_text <- get_error_text(model_text, msg)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_4.err",
    update = update_expected
  )
})

test_that("stray endif without matching if", {

  model_text <- glue::glue("
     x = 1;
     #endif
    ")

  err_text <- get_error_text(model_text)

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

  err_text <- get_error_text(model_text)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_6.err",
    update = update_expected
  )

  err_text_2 <- get_error_text(model_text, flags = "a")
  expect_equal(err_text, err_text_2)

  err_text_3 <- get_error_text(model_text, flags = c("a", "b"))
  expect_equal(err_text, err_text_3)

  err_text_4 <- get_error_text(model_text, flags = c("a", "b", "c"))
  expect_equal(err_text, err_text_4)
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

  err_text <- get_error_text(
    model_text,
    flags = paste0("a", 1:11, collapse = ",")
  )

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_7.err",
    update = update_expected
  )

  err_text_2 <- get_error_text(model_text)
  expect_equal(err_text, err_text_2)
})

test_that("#IF in included filename", {

  expect_error(
    mdl <- isis_mdl("mdl/cond_comp1.mdl", silent = TRUE)
  )

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

test_that("elseif after if", {

  model_text <- glue::glue("
     #if a
     x = 1;
     #else
     x = 2;
     #elseif b
       y = 2;
     #endif
    ")

  err_text <- get_error_text(model_text)

  expect_known_output(
    cat(err_text),
    file = "expected_output/cond_comp_error_10.err",
    update = update_expected
  )

  err_text_2 <- get_error_text(model_text, flags = "a")

  expect_equal(err_text, err_text_2)
})
