library(utils)
library(isismdl)
library(testthat)

context("fill_mdl_data for the ISLM model")

test_that("filmdt works correctly", {
  capture_output(mdl <- read_mdl("islm_model_solved.rds"))
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_values(names = "y", value = NA, period = "2015Q3/2016Q1")
  mdl2$set_values(names = "yd", value = NA, period = "2015Q4")
  report <- capture.output(mdl2$fill_mdl_data(report = "minimal"))
  expect_identical(report,
                   "Replaced a total of 4 missing/invalid values in identities")
  expect_equal(mdl2$get_data(), mdl$get_data())
})
