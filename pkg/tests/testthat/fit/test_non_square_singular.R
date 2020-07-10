library(isismdl)
library(testthat)

rm(list = ls())

context("fit nonsquare mdl singular")

source("../tools/convert_report.R")

mdl <- isis_mdl("mdl/non_square.mdl", period = 1000, silent = TRUE)

fit <- regts(matrix(c(1, 2), nrow = 1),
             period = 1000, names = paste0("w", 1:2))

rms <- rep(1, 3)
names(rms) <- paste0("x", 1:3)

mdl$set_fit(fit)
mdl$set_rms(rms)
mdl$set_fit_options(svdtest_tol = 1e-6, dbgopt = "prijac")

test_that("report correct",{


  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation not possible")

  expect_known_output(cat_report(convert_report(report)),
                                 "expected_output/non_square_singular_rep1.txt")

  })
