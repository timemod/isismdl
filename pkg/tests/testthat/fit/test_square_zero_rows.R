library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/solve_report_tools.R")
source("../tools/expect_known_output_multi.R")

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
  mdl2$set_fit_options(scale_method = "none", warn_ca = FALSE, store_jac = TRUE)

  # without row scaling simulation is not possible
  expect_warning(
    report1 <- capture.output(mdl2$solve()),
    "Simulation not possible"
  )
  expect_known_report(report1,
                      "expected_output/square_zero_rows_rep1.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)


  # without row scaling simulation is not possible
  expect_warning(
    report1a <- capture.output(
      mdl2$solve(fit_options = list(warn_zero_row = TRUE))
    ),
    "Simulation not possible"
  )
  expect_known_report(report1a,
                      "expected_output/square_zero_rows_rep1a.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)

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

  # svd_analysis
  fit_jac <- mdl2$get_fit_jac()
  result <- svd_analysis(fit_jac)
  expect_equal(result$rank, 5)
  expect_true(length(result$small_sv) == 1)
  expect_equal(result$lin_dep_rows, 2)
  expect_equal(result$lin_dep_cols, 1:6)
  expect_equal(result$zero_rows, 2)
  expect_equal(length(result$zero_cols), 0)

  result2 <- svd_analysis(fit_jac, zero_tol = 1e-12)
  expect_equal(result2$rank, 5)
  expect_true(length(result2$small_sv) == 1)
  expect_equal(result2$lin_dep_rows, 2)
  expect_equal(result2$lin_dep_cols, 1:6)
  expect_equal(length(result2$zero_rows), 0)
  expect_equal(length(result2$zero_cols), 0)
})

test_that("row with only zeros", {
  mdl2 <- mdl$copy()
  mdl2$set_fit_options(scale_method = "row")
  mdl2$set_param(list(r2 = c(0, 0, 0, 0, 0, 0)))
  expect_warning(
    report2 <- capture.output(mdl2$solve()),
    "Simulation not possible"
  )
  expect_known_report(report2,
                      "expected_output/square_zero_rows_rep2.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)

  mdl2$set_param(list(r3 = c(0, 0, 1e-9, 0, 0, 0)))
  expect_warning(
    report3 <- capture.output(
      mdl2$solve(fit_options = list(warn_zero_row = TRUE))
    ),
    "Simulation not possible"
  )
  expect_known_report(report3,
                      "expected_output/square_zero_rows_rep3.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)
})
