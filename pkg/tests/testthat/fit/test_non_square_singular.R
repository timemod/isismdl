library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/expect_known_output_multi.R")
source("../tools/solve_report_tools.R")

mdl <- isis_mdl("mdl/non_square.mdl", period = 1000, silent = TRUE)

fit <- regts(matrix(c(1, 2), nrow = 1),
             period = 1000, names = paste0("w", 1:2))

rms <- rep(1, 3)
names(rms) <- paste0("x", 1:3)

mdl$set_fit(fit)
mdl$set_rms(rms)
mdl$set_fit_options(store_jac = TRUE, dbgopt = "prijac")

test_that("report correct", {
  expect_warning(
    report <- capture.output(mdl$solve()),
    "Simulation not possible"
  )
  expect_known_report_multi(report,
                            "expected_output/non_square_singular_rep1",
                            update = update_expected)
})

test_that("jacobian correct", {
  expect_equal(
    mdl$get_fit_jac(),
    matrix(rep(1, 6), ncol = 3, nrow = 2,
           dimnames = list(c("w1", "w2"), c("x1", "x2", "x3")))
  )
})

test_that("svd analysis", {

  fit_jac <- mdl$get_fit_jac()
  result <- svd_analysis(fit_jac)
  expect_equal(result$rank, 1)
  expect_true(length(result$small_sv) == 1)
  expect_equal(result$lin_dep_rows, 1:2)
  expect_equal(result$lin_dep_cols, 1:3)
  expect_equal(length(result$zero_rows), 0)
  expect_equal(length(result$zero_cols), 0)
})

test_that("zero columns", {

  # Column x3 zero ----

  mdl2 <- mdl$copy()
  mdl2$set_fit_options(dbgopt = "noprijac")
  mdl2$set_param(list(r1 = c(1, 2, 0), r2 = c(2, 2, 0)))
  expect_silent(mdl2$solve(options = list(report = "none")))
  expect_equal(mdl2$get_solve_status(), "OK")
  expect_equal(mdl2$get_data(pattern = "^w[12345]"), mdl2$get_fit(),
               tolerance = 1e-6)

  # check fit jac
  fit_jac <- mdl2$get_fit_jac()

  expect_equal(
    fit_jac,
    matrix(c(1, 2, 2, 2, 0, 0), ncol = 3, nrow = 2,
           dimnames = list(c("w1", "w2"), c("x1", "x2", "x3")))
  )

  # svd analysis
  result <- svd_analysis(fit_jac)
  expect_output(print(result), "Full numerical rank")
  expect_equal(result$rank, 2)
  expect_true(length(result$small_sv) == 0)
  expect_null(result$U_small)
  expect_null(result$V_small)
  expect_equal(length(result$zero_rows), 0)
  expect_equal(result$zero_cols, 3)

  #  Column 3 zero, columns 1 and 2 equal -----

  mdl2$set_param(list(r1 = c(2, 2, 0), r2 = c(2, 2, 0)))
  expect_warning(
    report2 <- capture.output(mdl2$solve()),
    "Simulation not possible"
  )
  expect_known_report_multi(report2,
                            "expected_output/non_square_singular_rep2",
                            update = update_expected)

  # fit jac
  fit_jac <- mdl2$get_fit_jac()
  expect_equal(
    fit_jac,
    matrix(c(2, 2, 2, 2, 0, 0), ncol = 3, nrow = 2,
           dimnames = list(c("w1", "w2"), c("x1", "x2", "x3")))
  )

  # svd analysis
  result <- svd_analysis(fit_jac)
  expect_equal(result$rank, 1)
  expect_true(length(result$small_sv) == 1)
  expect_equal(result$lin_dep_rows, 1:2)
  expect_equal(result$lin_dep_cols, 1:2)
  expect_equal(length(result$zero_rows), 0)
  expect_equal(result$zero_cols, 3)

  expect_warning(
    report3 <- capture.output(
      mdl2$solve(fit_options = list(warn_zero_col = TRUE))
    ),
    "Simulation not possible"
  )
  expect_known_report_multi(report3,
                            "expected_output/non_square_singular_rep3",
                            update = update_expected,
                            replace_all_numbers = TRUE)

  # Zero colums 2 and 3 -----

  mdl2$set_param(list(r1 = c(1, 0, 0), r2 = c(2, 0, 0)))
  expect_warning(
    report4 <- capture.output(
      mdl2$solve(fit_options = list(warn_zero_col = FALSE))
    ),
    "Simulation not possible"
  )
  expect_known_report_multi(report4,
                            "expected_output/non_square_singular_rep4",
                            update = update_expected,
                            replace_all_numbers = TRUE)

  # Column 2 almost zero, column 3 exactly zero, warn_zero_col ----

  mdl2$set_param(list(r1 = c(1, 1e-8, 0), r2 = c(2, 0, 0)))
  expect_warning(
    report5 <- capture.output(
      mdl2$solve(fit_options = list(warn_zero_col = TRUE))
    ),
    "Simulation not possible"
  )
  expect_known_report_multi(report5,
                            "expected_output/non_square_singular_rep5",
                            update = update_expected,
                            replace_all_numbers = TRUE)
  # fit jac
  fit_jac <- mdl2$get_fit_jac()

  expect_equal(
    fit_jac,
    matrix(c(1, 2, 1e-8, 0, 0, 0), ncol = 3, nrow = 2,
           dimnames = list(c("w1", "w2"), c("x1", "x2", "x3")))
  )

  result <- svd_analysis(fit_jac, vector_tol = 1e-12)
  expect_equal(result$rank, 1)
  expect_true(length(result$small_sv) == 1)
  expect_equal(result$lin_dep_rows, 1:2)
  expect_equal(result$lin_dep_cols, 1:2)
  expect_equal(length(result$zero_rows), 0)
  expect_equal(result$zero_cols, 2:3)

  result2 <- svd_analysis(fit_jac, vector_tol = 1e-12, zero_tol = 1e-20)
  expect_equal(result2$zero_cols, 3)
})
