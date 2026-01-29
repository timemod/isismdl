library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

test_that("fill_mdl_data for identities", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_values(names = "y", value = NA, period = "2015Q3/2016Q1")
  mdl2$set_values(names = "yd", value = NA, period = "2015Q4")
  expect_output(
    mdl2$fill_mdl_data(report = "minimal"),
    "Replaced a total of 4 missing/invalid values in identities",
    fixed = TRUE
  )
  expect_equal(mdl2$get_data(), mdl$get_data())
})

test_that("fill_mdl_data with frmls", {

  mdl2 <- mdl$copy()
  expect_output(
    mdl2$fill_mdl_data(period =  "2015Q2/2016Q1", report = "minimal",
                       include_frmls = TRUE),
    "No missing/invalid values replaced",
    fixed = TRUE
  )

  mdl2$set_values(names = "c", value = NA, period = "2015Q3/2016Q1")
  mdl2$set_ca_values(c(NA, 1, 2), names = "c", period = "2015Q3/2016Q1")

  mdl2$set_eq_status("inactive", names = "c")
  expect_output(
    mdl2$fill_mdl_data(period =  "2015Q2/2016Q1", report = "minimal",
                       include_frmls = TRUE),
    "No missing/invalid values replaced",
    fixed = TRUE
  )
  mdl2$set_eq_status("active", names = "c")
  expect_output(
    mdl2$fill_mdl_data(period =  "2015Q2/2016Q1", report = "minimal",
                       include_frmls = TRUE),
    "Replaced a total of 2 missing/invalid values",
    fixed = TRUE
  )
})

test_that("errors", {
  expect_error(
    mdl$run_eqn(period = "2014M1/2014M6"),
    paste("The specified period (2014Q1/2014Q2) lies outside the range of the",
          "data period (2015Q1/2016Q3)."),
    fixed = TRUE
  )
})
