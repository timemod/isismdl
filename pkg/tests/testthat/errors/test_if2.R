library(isismdl)
library(testthat)

context("logical/numerical type errors")

test_that("output function isis_mdl is correct", {
  msg <- "Model not correct \\(see output\\)"
  report <- capture_output(expect_error(mdl <- isis_mdl("mdl/if2"), msg))
  #cat(report)
  expect_equal_to_reference(report, "expected_output/if2.rds")
})
