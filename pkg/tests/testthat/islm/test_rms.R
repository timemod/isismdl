library(testthat)
library(isismdl)

rm(list = ls())

context("rms values for ISLM model")

# prepare rms values and fit targets
rms_values <- c(c = 5.0, t = 2, i = 21, md = 2)
rms_values_sorted <- rms_values[order(names(rms_values))]

capture_output(mdl <- read_mdl("islm_model.ismdl"))
mdl2 <- mdl$copy()

test_that("get_rms with all positive numbers", {
  mdl2$set_rms(rms_values)
  expect_identical(mdl$get_rms(), numeric(0))
  expect_identical(mdl2$get_rms(), rms_values_sorted)
  mdl2$write_mdl("tmp.ismdl")
  capture.output(mdl3 <- read_mdl("tmp.ismdl"))
  unlink("tmp.ismdl")
  expect_identical(mdl3$get_rms(), rms_values_sorted)
})

test_that("rms values after clear_fit", {
  mdl_clear <- mdl2$clone(deep = TRUE)
  mdl_clear$clear_fit()
  expect_identical(mdl_clear$get_rms(), numeric(0))
})

rms_values2 <- c(c = 5.0, t = NA, i = 0, md = 2)

mdl4 <- mdl$clone(deep = TRUE)
mdl4$set_rms(rms_values2)

test_that("get_rms with NA and 0 values", {
  expect_identical(mdl4$get_rms(), rms_values2[c("c", "md")])
  mdl4$write_mdl("tmp.ismdl")
  capture.output(mdl5 <- read_mdl("tmp.ismdl"))
  unlink("tmp.ismdl")
  expect_identical(mdl5$get_rms(), rms_values2[c("c", "md")])
})

rms_values3 <- c(c = NA, t = 0)

mdl6 <- mdl$clone(deep = TRUE)
mdl6$set_rms(rms_values3)

test_that("get_rms with NA and 0 values", {
  expect_identical(mdl6$get_rms(), numeric(0))
  mdl6$write_mdl("tmp.ismdl")
  capture.output(mdl7 <- read_mdl("tmp.ismdl"))
  unlink("tmp.ismdl")
  expect_identical(mdl7$get_rms(), numeric(0))
})

test_that("set_rms name_err", {
  rms_values_before <- mdl6$get_rms()
  rms_values_new <- rms_values_before * 2
  rms_values_new["y"] <- 2
  msg <- "\"y\" is not a frml variable\\."
  expect_error(mdl6$set_rms(rms_values_new, name_err = "stop"), msg)
  rms_values_new["x"] <- 3
  msg <- "The following names are no frml variables: \"y\", \"x\"\\."
  expect_warning(mdl6$set_rms(rms_values_new, name_err = "warn"), msg)
  expect_silent(mdl6$set_rms(rms_values_new, name_err = "silent"))
  expect_silent(mdl6$set_rms(rms_values_new))
  expect_equal(mdl6$get_rms(), rms_values_before * 2)
})

test_that("errors", {
  expect_error(mdl6$set_rms(c(a = "jan")),
               "Argument values is not a numeric vector")
  expect_error(mdl6$set_rms(2),
               "Argument values is not a named numeric vector")
})




