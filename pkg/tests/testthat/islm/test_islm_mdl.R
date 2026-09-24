library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

test_that("islm_mdl silent", {

  expect_silent(mdl <- islm_mdl(silent = TRUE))
  expect_s3_class(mdl, "IsisMdl")

  expect_output(mdl_noisy <- islm_mdl(silent = FALSE))
  expect_s3_class(mdl_noisy, "IsisMdl")

  expect_output(mdl_default <- islm_mdl())
  expect_s3_class(mdl_default, "IsisMdl")

  expect_silent(mdl_per <- islm_mdl(period = "2015Q1/2016Q4", silent = TRUE))
  expect_s3_class(mdl_per, "IsisMdl")
  expect_equal(mdl_per$get_period(), as.period_range("2015Q1/2016Q4"))
})
