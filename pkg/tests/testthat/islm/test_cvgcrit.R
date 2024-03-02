library(isismdl)
library(testthat)

rm(list = ls())


rep1 <- capture_output(mdl <- read_mdl("islm_model.ismdl"))
rep2 <- capture_output(mdl_solved <- read_mdl("islm_model_solved.ismdl"))

mdl$set_cvgcrit(0.01)
mdl$set_cvgcrit(0.005, names = c("c", "r"))

expected_result <- c(c = 0.005, g = 0.01, i = 0.01, md = 0.01, ms = 0.01,
                     r = 0.005, t = 0.01, y = 0.01, yd = 0.01)

test_that("get_cvgcrit gives correct result", {
  expect_identical(mdl$get_cvgcrit(), expected_result)
})

test_that("also ok after copy and reading /writign to file", {
  mdl2 <- mdl$copy()

  expect_identical(mdl2$get_cvgcrit(), expected_result)

  ismdl_file <- tempfile(fileext = ".ismdl")
  mdl$write_mdl(ismdl_file)
  rep <- capture.output(mdl3 <- read_mdl(ismdl_file))
  expect_identical(mdl3$get_cvgcrit(), expected_result)
})

test_that("effec on solving", {
  mdl$solve(options = list(report = "none"))
  dif1 <- tsdif(mdl$get_data(), mdl_solved$get_data(), fun = cvgdif, tol = 1e-6)
  expect_false(dif1$equal)
  dif2 <- tsdif(mdl$get_data(), mdl_solved$get_data(), fun = cvgdif, tol = 1e-2)
  expect_true(dif2$equal)
  mdl$set_cvgcrit(1e-8)$solve(options = list(report = "none"))
  dif3 <- tsdif(mdl$get_data(), mdl_solved$get_data(), fun = cvgdif, tol = 1e-6)
  expect_true(dif3$equal)
})

test_that("errors", {
  msg <- "value should be a single numerical value"
  expect_error(mdl$set_cvgcrit(NA), msg)
  expect_error(mdl$set_cvgcrit("2"), msg)
  expect_error(mdl$set_cvgcrit(1:2), msg)

  msg <- "value should not be a NA"
  expect_error(mdl$set_cvgcrit(NA_real_), msg)

  msg <- "value should be a finite number"
  expect_error(mdl$set_cvgcrit(1/0), msg)
})



