# Syntax errors for conditional compilation

library(isismdl)
library(testthat)

rm(list = ls())

test_that("empty model text", {

  omsg <- "0 equations processed"
  emsg <- "Model has no equations"

  # model text of length 0
  expect_output(
    expect_error(
      mdl <- isis_mdl(model_text = ""),
      emsg
    ),
    omsg
  )

  # only comments
  expect_output(
    expect_error(
      mdl <- isis_mdl(model_text = "? This is a comment"),
      emsg
    ),
    omsg
  )

  # only a function
  expect_output(
    expect_error(
      mdl <- isis_mdl(model_text = "function f(x) = x;"),
      emsg
    ),
    omsg
  )

})

