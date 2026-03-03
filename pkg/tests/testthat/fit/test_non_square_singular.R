library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/convert_report.R")
source("../tools/get_platform_variant.R")
source("../tools/expect_known_output_multi.R")

platform_variant <- get_platform_variant()

mdl <- isis_mdl("mdl/non_square.mdl", period = 1000, silent = TRUE)

fit <- regts(matrix(c(1, 2), nrow = 1),
             period = 1000, names = paste0("w", 1:2))

rms <- rep(1, 3)
names(rms) <- paste0("x", 1:3)

mdl$set_fit(fit)
mdl$set_rms(rms)
mdl$set_fit_options(svdtest_tol = 1e-6, dbgopt = "prijac")

test_that("report correct", {
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation not possible")
  expect_known_output_multi(cat_report(convert_report(report)),
                            "expected_output/non_square_singular_rep1",
                            update = update_expected)
})


test_that("zero columns", {
  mdl2 <- mdl$copy()
  mdl2$set_fit_options(dbgopt = "noprijac")
  mdl2$set_param(list(r1 = c(1, 2, 0), r2 = c(2, 2, 0)))
  expect_silent(mdl2$solve(options = list(report = "none")))
  expect_equal(mdl2$get_solve_status(), "OK")
  expect_equal(mdl2$get_data(pattern = "^w[12345]"), mdl2$get_fit(),
               tolerance = 1e-6)

  mdl2$set_param(list(r1 = c(2, 2, 0), r2 = c(2, 2, 0)))
  expect_warning(report2 <- capture.output(mdl2$solve()),
                 "Simulation not possible")
  expect_known_output_multi(cat_report(convert_report(report2, replace_all_numbers = TRUE)),
                            "expected_output/non_square_singular_rep2",
                            update = update_expected)

  expect_warning(
    report3 <- capture.output(mdl2$solve(fit_options = list(warn_zero_col = TRUE))),
    "Simulation not possible"
  )
  expect_known_output_multi(cat_report(convert_report(report3, replace_all_numbers = TRUE)),
                            "expected_output/non_square_singular_rep3",
                            update = update_expected)

  mdl2$set_param(list(r1 = c(1, 0, 0), r2 = c(2, 0, 0)))
  expect_warning(report4 <- capture.output(mdl2$solve(fit_options =
                                                        list(warn_zero_col = FALSE))),
                 "Simulation not possible")
  expect_known_output_multi(cat_report(convert_report(report4, replace_all_numbers = TRUE)),
                            "expected_output/non_square_singular_rep4",
                            update = update_expected)

  mdl2$set_param(list(r1 = c(1, 1e-8, 0), r2 = c(2, 0, 0)))
  expect_warning(report5 <- capture.output(mdl2$solve(fit_options =
                                                        list(warn_zero_col = TRUE))),
                 "Simulation not possible")
  expect_known_output_multi(cat_report(convert_report(report5, replace_all_numbers = TRUE)),
                            "expected_output/non_square_singular_rep5",
                            update = update_expected)
})
