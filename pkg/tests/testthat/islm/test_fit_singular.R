library(testthat)
library(isismdl)

rm(list = ls())

update_expected <- FALSE

source("../tools/solve_report_tools.R")

# prepare rms values and fit targets
rms_values <- c(c = 5.0, i = 21, md = 2)
y <- regts(985, start = "2015Q2")
yd <- regts(800, start = "2015Q2")
r <- regts(3.5, start = "2015Q2")
fit_targets <- cbind(y, yd, r)

islm_model <- read_mdl("islm_model.ismdl", silent = TRUE)

islm_model$set_fit(fit_targets)
islm_model$set_rms(rms_values)

test_that("Correct type of report created", {
  expect_warning(
    report <- capture.output(
      islm_model$solve(fit_options = list(accurate_jac = TRUE, store_jac = TRUE,
                                          dbgopt = "prijac"))
    ),
    "Simulation not possible"
  )

  expect_known_report(report,
                      "expected_output/fit_singular_rep1.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)

  fit_jac <- islm_model$get_fit_jac()
  expect_known_value(fit_jac,
                     file =  "expected_output/fit_singular.rds",
                     update = update_expected)
})
