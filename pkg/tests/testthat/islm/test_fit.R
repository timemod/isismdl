library(testthat)
library(isismdl)

context("fit for ISLM model")

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
islm_model$set_fit(fit)

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
