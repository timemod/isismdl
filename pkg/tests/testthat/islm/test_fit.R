library(testthat)
library(isismdl)

context("fit for ISLM model")

source("../tools/convert_report.R")

# prepare rms values and fit targets
rms_values <- c(c = 5.0, t = 2, i = 21, md = 2)
i <- regts(200, start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fit_targets <- cbind(y, i)
ts_labels(fit_targets) <- c("income", "investment")

isis_result <- as.regts(read.csv("isi/fit_zealous.csv"), time_column = 1)

islm_model <- read_mdl("islm_model.ismdl", silent = TRUE)

islm_model$set_fit(fit_targets)
islm_model$set_rms(rms_values)

test_that("Testing get_fit", {
  expect_identical(islm_model$get_fit(), fit_targets[, c("i", "y")])
})

test_that("Comparing the results of solve for the lazy fit procedure", {
  islm_model$solve(options = list(report = "none"))
  dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result,
               tol = 1e-8, fun = cvgdif)
  expect_identical(dif$missing_names1, character(0))
  expect_identical(dif$missing_names2, character(0))
  expect_identical(dif$difnames, character(0))
})

#  now remove fit targets and solve again
fit <- islm_model$get_data()[islm_model$get_period(), ]
fit[] <- NA

test_that("set_fit", {
  msg <- "The following names are no endogenous variables: \"g\", \"ms\"\\."
  expect_error(islm_model$set_fit(fit, name_err = "stop"), msg)
  expect_warning(islm_model$set_fit(fit, name_err = "warn"), msg)
  expect_silent(islm_model$set_fit(fit, name_err = "silent"))
})

islm_model$solve(options = list(report = "none"))
dif <- tsdif(islm_model$get_data(period = "2015Q2/2016Q3"), isis_result,
             tol = 1e-6, fun = cvgdif)

test_that("Comparing solve after removing the fit targets", {
  expect_identical(dif$missing_names1, character(0))
  expect_identical(dif$missing_names2, character(0))
  expect_identical(dif$difnames, character(0))
})

# now also set all CAs to zero
islm_model$set_ca_values(0)
islm_model$solve(options = list(report = "none"))
isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)
dif <- tsdif(islm_model$get_data(period = islm_model$get_period()), isis_result,
             tol = 1e-6, fun = cvgdif)
#print(dif)

test_that("Comparing solve after removing fit targets and setting the
          CAs to 0", {
  expect_identical(dif$missing_names1, character(0))
  expect_identical(dif$missing_names2, character(0))
  expect_identical(dif$difnames, character(0))
})

test_that("reduced model period", {
  per <- period_range("2015q4/2016q3")
  mdl <- islm_model$copy()$set_period(per)
  fit <- lag(fit_targets, -2)
  mdl$set_fit(fit)
  expect_equal(mdl$get_fit(), fit[ , c("i", "y")])
  mdl$solve(options = list(report = "none"))
  data <- mdl$get_data(period = per, names = c("i", "y"))
  expected_data <- update_ts(data, fit, method = "updval")
  expect_equal(data, expected_data)
})


test_that("zero_ca", {
  mdl <- islm_model$copy()$set_fit(fit_targets)
  mdl$set_solve_options(report = "none")

  mdl$solve()
  expect_equal(mdl$get_solve_status(), "OK")
  mdl$solve(fit_options = list(maxiter = 1, zero_ca = FALSE))
  expect_equal(mdl$get_solve_status(), "OK")
  expect_warning(mdl$solve(fit_options = list(maxiter = 1, zero_ca = TRUE)),
                 "Simulation stopped")
  expect_equal(mdl$get_solve_status(), "Simulation stopped")
})


test_that("fixed fit instruments (1)", {
  mdl <- islm_model$copy()
  y <- regts(200, period = '2015q2/2015q4')
  r <- regts(3.5, period = '2015q2/2015q4')
  fit_targets <- cbind(y, r)
  mdl$set_fit(fit_targets)
  mdl$fix_variables("c", period = "2015q3")
  mdl$set_fit_options(zealous = FALSE, warn_ca = FALSE)
  mdl$solve(options = list(report = "none"))
  expect_equal(mdl$get_solve_status(), "OK")
  report <- capture.output(mdl$solve())
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_1a.txt")

  fit_fixed <- cbind(mdl$get_fit(), mdl$get_fix())
  data <- mdl$get_data(names = colnames(fit_fixed), period = get_period_range(fit_fixed))
  expected_result <- update_ts(fit_fixed, data, method = "updna")[ , colnames(fit_fixed)]
  expect_equal(data, expected_result, tol = 1e-3)

  mdl$fix_variables(c("i", "md"), period = "2015q3")
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_1b.txt")
  expect_equal(mdl$get_solve_status(), "Simulation stopped")

  mdl$fix_variables("t", period = "2015q3")
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_1c.txt")
  expect_equal(mdl$get_solve_status(), "Simulation stopped")

  expect_warning(report <- capture.output(mdl$solve(period = "2015q3")),
                 "Simulation not possible")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_1d.txt")
})

test_that("fixed fit instruments (2)", {
  mdl <- islm_model$copy()
  y <- regts(200, period = '2015q2/2015q4')
  r <- regts(3.5, period = '2015q2/2015q4')
  fit_targets <- cbind(y, r)
  mdl$set_fit(fit_targets)
  mdl$fix_variables("c")
  mdl$set_fit_options(zealous = FALSE, warn_ca = FALSE)
  mdl$solve(options = list(report = "none"))
  expect_equal(mdl$get_solve_status(), "OK")
  report <- capture.output(mdl$solve())
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_2a.txt")

  fit_fixed <- cbind(mdl$get_fit(), mdl$get_fix())
  data <- mdl$get_data(names = colnames(fit_fixed), period = get_period_range(fit_fixed))
  expected_result <- update_ts(fit_fixed, data, method = "updna")[ , colnames(fit_fixed)]
  expect_equal(data, expected_result, tol = 1e-3)

  mdl$fix_variables(c("i", "md"))
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_2b.txt")
  expect_equal(mdl$get_solve_status(), "Simulation stopped")

  mdl$fix_variables("t")
  expect_warning(report <- capture.output(mdl$solve()),
                "Simulation not possible")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_2c.txt")
  expect_equal(mdl$get_solve_status(), "Simulation not possible")
})

test_that("fixed fit instruments (3)", {
  mdl <- islm_model$copy()
  y <- regts(200, period = '2015q2/2015q4')
  r <- regts(3.5, period = '2015q2/2015q4')
  fit_targets <- cbind(y, r)
  mdl$set_fit(fit_targets)
  mdl$set_eq_status("inactive", names = "c")
  mdl$set_fit_options(zealous = FALSE, warn_ca = FALSE)
  mdl$solve(options = list(report = "none"))
  expect_equal(mdl$get_solve_status(), "OK")
  report <- capture.output(mdl$solve())
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_3a.txt")

  fit_fixed <- cbind(mdl$get_fit(), mdl$get_fix())
  data <- mdl$get_data(names = colnames(fit_fixed), period = get_period_range(fit_fixed))
  expected_result <- update_ts(fit_fixed, data, method = "updna")[ , colnames(fit_fixed)]
  expect_equal(data, expected_result, tol = 1e-3)

  mdl$fix_variables(c("i", "md"), period = "2015q3")
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_3b.txt")
  expect_equal(mdl$get_solve_status(), "Simulation stopped")
})

test_that("fixed fit instruments (4)", {
  mdl <- islm_model$copy()
  y <- regts(200, period = '2015q2/2015q4')
  r <- regts(3.5, period = '2015q2/2015q4')
  fit_targets <- cbind(y, r)
  mdl$set_fit(fit_targets)
  mdl$set_eq_status("inactive", names = "c")
  mdl$set_fit_options(zealous = FALSE, warn_ca = FALSE)
  mdl$solve(options = list(report = "none"))
  expect_equal(mdl$get_solve_status(), "OK")
  report <- capture.output(mdl$solve())
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_4a.txt")

  fit_fixed <- cbind(mdl$get_fit(), mdl$get_fix())
  data <- mdl$get_data(names = colnames(fit_fixed), period = get_period_range(fit_fixed))
  expected_result <- update_ts(fit_fixed, data, method = "updna")[ , colnames(fit_fixed)]
  expect_equal(data, expected_result, tol = 1e-3)

  mdl$fix_variables(c("i", "md"))
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_4b.txt")
  expect_equal(mdl$get_solve_status(), "Simulation stopped")

  mdl$fix_variables("t", period = "2015q3")
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation stopped")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_4c.txt")

  mdl$fix_variables("t")
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation not possible")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_fixed_instr_4d.txt")
  expect_equal(mdl$get_solve_status(), "Simulation not possible")

})

test_that("deactivated equations (1)", {
  mdl <- islm_model$copy()
  y <- regts(200, period = '2015q2/2015q4')
  r <- regts(3.5, period = '2015q2/2015q4')
  fit_targets <- cbind(y, r)
  mdl$set_fit(fit_targets)
  mdl$set_eq_status("inactive", names = c("c", "i", "md", "t"))
  expect_warning(report <- capture.output(mdl$solve()),
                 "Simulation not possible")
  expect_known_output(cat_report(convert_report(report,
                                                replace_all_numbers = TRUE)),
                      "expected_output/fit_deact_1a.txt")
  expect_equal(mdl$get_solve_status(), "Simulation not possible")
})

