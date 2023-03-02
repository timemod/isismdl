library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/convert_report.R")


mdl <- isis_mdl("mdl/square.mdl", period = 1000, silent = TRUE)

fit <- regts(matrix(c(0.412, 0.202, 0.813, 0.642, 0.123, 0.245), nrow = 1),
             period = 1000, names = paste0("w", 1:6))

rms <- rep(1, 6)
names(rms) <- paste0("x", 1:6)

mdl$set_fit(fit)
mdl$set_rms(rms)

test_that("row with almost zeros", {
  mdl2 <- mdl$copy()
  mdl2$set_param(list(r2 = c(0, 0, 1e-9, 0, 0, 0)))
  mdl2$set_fit_options(scale_method = "none", warn_ca = FALSE,
                       svdtest_tol = 1e-8)

  # without row scaling simulation is not possible
  expect_warning(report1 <- capture.output(mdl2$solve()),
                 "Simulation not possible")
  expect_known_output(cat_report(convert_report(report1,
                                                replace_all_numbers = TRUE)),
                      "expected_output/square_zero_rows_rep1.txt",
                      update = update_expected)

  # without row scaling simulation is not possible
  expect_warning(report1a <- capture.output(
     mdl2$solve(fit_options = list(warn_zero_row = TRUE))),
                 "Simulation not possible")
  expect_known_output(cat_report(convert_report(report1a,
                                                replace_all_numbers = TRUE)),
                      "expected_output/square_zero_rows_rep1a.txt",
                      update = update_expected)

  # with row scaling simulation is possible
  mdl2$set_fit_options(scale_method = "row")
  expect_silent(mdl2$solve(options = list(report = "none")))
  expect_identical(mdl2$get_solve_status(), "OK")
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit, tolerance = 1e-6)

  # with chkjac = FALSE, simulation is also possible
  mdl2$set_fit_options(scale_method = "none", chkjac = FALSE)
  expect_silent(mdl2$solve(options = list(report = "none")))
  expect_identical(mdl2$get_solve_status(), "OK")
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit, tolerance = 1e-6)
})

test_that("row with only zeros", {
  mdl2 <- mdl$copy()
  mdl2$set_fit_options(scale_method = "row")
  mdl2$set_param(list(r2 = c(0, 0, 0, 0, 0, 0)))
  expect_warning(report2 <- capture.output(mdl2$solve()),
                 "Simulation not possible")
  expect_known_output(cat_report(convert_report(report2,
                                                replace_all_numbers = TRUE)),
                      "expected_output/square_zero_rows_rep2.txt",
                      update = update_expected)

  mdl2$set_param(list(r3 = c(0, 0, 1e-9, 0, 0, 0)))
  expect_warning(report3 <- capture.output(mdl2$solve(fit_options =
                                                        list(warn_zero_row = TRUE))),
                 "Simulation not possible")
  expect_known_output(cat_report(convert_report(report3,
                                                replace_all_numbers = TRUE)),
                      "expected_output/square_zero_rows_rep3.txt",
                      update = update_expected)
})
