# Test type mismatch in logical operations

library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

test_that("output function isis_mdl is correct", {
  msg <- "Model not correct \\(see output\\)"
  report <- capture_output(
    expect_error(
      isis_mdl("mdl/if2"),
      msg
    )
  )
  expect_known_output(cat(report), "expected_output/if2.txt",
                      update = update_expected)
})
