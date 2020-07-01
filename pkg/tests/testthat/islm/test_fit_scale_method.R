library(testthat)
library(isismdl)

rm(list = ls())

context("scale_method fit")

period <- period("2020q2")

mdl <- isis_mdl("mdl/islm_euro.mdl", period = period, silent = TRUE, )

# set exogenous variables
mdl$set_values(210000000, names = "g", period = period)
mdl$set_values(200000000, names = "ms", period = period)

# set lags and / or feedback variables
mdl$set_values(5, names = "r")
mdl$set_values(980000000, names = "y")
mdl$set_values(789000000, names = "yd")

test_that("solve model without fit", {
  mdl$solve(options = list(report = "none"))
  expect_equal(mdl$get_solve_status(), "OK")
})

#
# prepare rms values and fit targets

mdl$set_rms(c(c = 50000000 , t = 20000000, i = 13000000, md = 20000000))
y <- regts(1000000000, start = period)
r <- regts(2.5, start = period)
fit_targets <- cbind(y, r)
mdl$set_fit(fit_targets)

test_that("model cannot be solved with scale_method = 'none'", {
  mdl$set_fit_options(scale_method = "none", svdtest_tol = 1e-8)
  expect_output(  # we expect output because of the svd test
    expect_warning(
      mdl$solve(options = list(report = "none")),
      "Simulation not possible")
  )
})

test_that("model can be solved with scale_method = 'row", {
  mdl$set_fit_options(scale_method = "row")
  expect_silent(mdl$solve(options = list(report = "none")))
  expect_equal(mdl$get_solve_status(), "OK")
  expect_equal(mdl$get_data(names = c("y", "r"), period = period),
               fit_targets)
})
