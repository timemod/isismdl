# Test model compilation for a model with a long line.

# The maximim line length of a buffer is 2000.

library(isismdl)
library(testthat)
library(readr)

rm(list = ls())

test_that("syntax errors for model with long line", {

  n <- 400
  model_lines <- c(
    paste0("x = ", paste(1:n, collapse = " + "), ";")
  )

  model_text <- paste(model_lines, collapse = "\n")

  per <- period("2015Q1")
  mdl <- isis_mdl(model_text = model_text, period = per, silent = TRUE)
  mdl$solve(options = list(report = "none"))
  expect_equal(
    mdl$get_data(),
    regts(matrix(sum(1:n), ncol = 1), period = per, names = "x")
  )

})
