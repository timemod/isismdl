library(isismdl)
library(testthat)
library(readr)

update_expected <- FALSE


test_that("output function isis_mdl is correct", {
  msg <- "Model not correct \\(see output\\)"
  report <- capture_output(expect_error(mdl <- isis_mdl("mdl/ulfunc2"), msg))
  #cat(report)
  expect_known_output(cat(report), "expected_output/ulfunc2.txt",
                      update = update_expected)
})
