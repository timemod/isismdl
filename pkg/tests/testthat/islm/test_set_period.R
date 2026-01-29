library(isismdl)
library(testthat)

rm(list = ls())

mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

test_that("set_period", {
  mdl2 <- mdl$copy()
  mdl2$set_values(999, names = "c", period = "2015Q2/2016Q3")
  mdl2$set_period("2015Q4/2016Q2")
  mdl2$solve(options = list(report = "none"))

  data_expected <- mdl$get_data()
  data_expected["2015Q2/2015Q3", "c"] <- 999
  data_expected["2016Q3", "c"] <- 999
  expect_equal(mdl2$get_data(), data_expected)
})

test_that("errors", {
  mdl2 <- mdl$copy()
  expect_error(
    mdl2$set_period("2016"),
    paste("The specified period (2016) has a different frequency than the",
          "data period (2015Q1/2016Q3)."),
    fixed = TRUE
  )

  expect_error(
    mdl2$set_period("2016q1/2016q4"),
    paste("The specified period (2016Q1/2016Q4) is not compatible with the data",
          "period (2015Q1/2016Q3). The period should lie within the range",
          "2015Q2/2016Q3."),
    fixed = TRUE
  )

  emsg <- "period should have a lower and upper bound"
  expect_error(mdl2$set_period("2016q1/"), emsg, fixed = TRUE)
  expect_error(mdl2$set_period("/2016q2"), emsg, fixed = TRUE)

  expect_true(isTRUE(all.equal(mdl, mdl2)))
})
