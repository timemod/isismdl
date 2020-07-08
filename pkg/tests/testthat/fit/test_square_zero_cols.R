library(isismdl)
library(testthat)

rm(list = ls())

source("../tools/convert_report.R")

context("fit square mdl zero columns")

mdl <- isis_mdl("mdl/square.mdl", period = 1000, silent = TRUE)

fit <- regts(matrix(c(0.412, 0.202, 0.813, 0.642, 0.123, 0.245), nrow = 1),
             period = 1000, names = paste0("w", 1:6))

rms <- rep(1, 6)
names(rms) <- paste0("x", 1:6)

mdl$set_fit(fit)
mdl$set_rms(rms)
param <- mdl$get_param()

test_that("column with zeros", {

  mdl2 <- mdl$copy()

  # make third column exacty zero
  param_new <- sapply(param, FUN = function(x) {x[3] <- 0; return(x)},
                      simplify = FALSE)
  mdl2$set_param(param_new)
  mdl2$set_fit_options(scale_method = "none", warn_ca = FALSE,
                      dbgopt = "prijac", warn_zero_col = TRUE,
                      svdtest_tol = 0)
#  mdl2$set_solve_options(report = "none")
  expect_warning(report <- capture.output(mdl2$solve()),
                 "Simulation not possible")

  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/square_zero_cols_rep1.txt")

  # make third column almost zero, and second row zero
  param_new$r3[3] <- 1e-12
  param_new$r2[] <- 0
  mdl2$set_param(param_new)
  expect_warning(report <- capture.output(mdl2$solve()),
                 "Simulation not possible")

  expect_known_output(cat_report(convert_report(report)),
                      "expected_output/square_zero_cols_rep2.txt")

  expect_warning(report <- capture.output(mdl2$solve(fit_options =
                                                       list(warn_zero_col = FALSE))),
                 "Simulation not possible")
  expect_known_output(cat_report(convert_report(report)),
                      "expected_output/square_zero_cols_rep3.txt")

})

test_that("zero row and one less fit target", {
  mdl2 <- mdl$copy()

  # make third column exacty zero
  param_new <- sapply(param, FUN = function(x) {x[3] <- 0; return(x)},
                      simplify = FALSE)
  mdl2$set_param(param_new)
  mdl2$set_fit_values(NA, names = "w6")
  mdl2$set_fit_options(warn_zero_col = TRUE)
  expect_silent(mdl2$solve(options = list(report = "none")))
  expect_equal(mdl2$get_data(pattern = "^w(12345)"), mdl2$g, tolerance = 1e-6)
})
