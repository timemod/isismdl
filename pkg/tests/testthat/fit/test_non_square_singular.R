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
mdl$set_fit_options(dbgopt = c("prijac", "storejac"))

test_that("report correct", {
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation not possible")
  expect_known_output_multi(cat_report(convert_report(report)),
                            "expected_output/non_square_singular_rep1",
                            update = update_expected)
})

test_that("jacobian correct", {
  fit_jac <- mdl$get_fit_jac()
  expect_equal(
    fit_jac,
    matrix(rep(1, 6), ncol = 3, nrow = 2,
           dimnames = list(c("w1", "w2"), c("x1", "x2", "x3")))
  )
})
