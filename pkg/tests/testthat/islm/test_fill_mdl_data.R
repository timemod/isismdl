library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

test_that("filmdt works correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_values(names = "y", value = NA, period = "2015Q3/2016Q1")
  mdl2$set_values(names = "yd", value = NA, period = "2015Q4")
  report <- capture.output(mdl2$fill_mdl_data(report = "minimal"))
  expect_identical(report,
                   "Replaced a total of 4 missing/invalid values in identities")
  expect_equal(mdl2$get_data(), mdl$get_data())
})

test_that("fill_mdl_data with frmls", {

  mdl2 <- mdl$copy()
  expect_output(
    mdl2$fill_mdl_data(period =  "2015Q2/2016Q1", report = "minimal",
                       idents_only = FALSE),
    "No missing/invalid values replaced",
    fixed = TRUE
  )

  mdl2$set_values(names = "c", value = NA, period = "2015Q3/2016Q1")
  mdl2$set_ca_values(c(NA, 1, 2), names = "c", period = "2015Q3/2016Q1")

  mdl2$set_eq_status("inactive", names = "c")
  expect_output(
    mdl2$fill_mdl_data(period =  "2015Q2/2016Q1", report = "minimal",
                       idents_only = FALSE),
    "No missing/invalid values replaced",
    fixed = TRUE
  )
  mdl2$set_eq_status("active", names = "c")
  expect_output(
    mdl2$fill_mdl_data(period =  "2015Q2/2016Q1", report = "minimal",
                       idents_only = FALSE),
    "Replaced a total of 2 missing/invalid values",
    fixed = TRUE
  )
})
