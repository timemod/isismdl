library(isismdl)
library(testthat)

rm(list = ls())

context("fit square mdl 1")

capture_output(mdl <- isis_mdl("mdl/square.mdl", period = 1000))

fit <- regts(matrix(c(0.412, 0.202, 0.813, 0.642, 0.123, 0.245), nrow = 1),
             period = 1000, names = paste0("w", 1:6))

rms <- rep(1, 6)
names(rms) <- paste0("x", 1:6)

mdl$set_fit(fit)
mdl$set_rms(rms)

test_that("test1 (non-singular case)", {

  mdl2 <- mdl$copy()
  expect_silent(mdl2$solve(options = list(report = "none")))
  expect_identical(mdl2$get_solve_status(), "OK")
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit)
})

test_that("test2 (singular case)", {

  mdl_singular <- mdl$copy()
  params <- mdl_singular$get_param(names = c("r3", "r5"))
  params$r5 <- params$r3 + 1e-6 * params$r5
  mdl_singular$set_param(params)

  mdl2 <- mdl_singular$copy()
  expect_output(mdl2$solve(options = list(report = "none"),
                           fit_options = list(svdtest_tol = 1e-6)))
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit)

  mdl2 <- mdl_singular$copy()
  expect_silent(mdl2$solve(options = list(report = "none"),
                fit_options = list(svdtest_tol = 1e-10)))
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit)

  mdl2 <- mdl_singular$copy()
  mdl2$set_fit_options(svdtest_tol = 1e-6)
  expect_output(mdl2$solve(options = list(report = "none")))
})

test_that("test2 (severer singular case, solution not possible)", {

  mdl_singular <- mdl$copy()
  params <- mdl_singular$get_param(names = c("r3", "r5"))
  params$r5 <- params$r3 + 1e-12 * params$r5
  mdl_singular$set_param(params)

  mdl2 <- mdl_singular$copy()
  expect_warning(expect_output(mdl2$solve(options = list(report = "none"),
                           fit_options = list(svdtest_tol = 1e-6))))
  expect_identical(mdl2$get_solve_status(), "Simulation not possible")
  expect_false(isTRUE(all.equal(mdl2$get_data(pattern = "^w\\d"), fit)))

  # TODO: isismdl option nochkjac implementeren
})

test_that("row with almost zeros", {
  mdl2 <- mdl$copy()
  mdl2$set_param(list(r2 = c(0, 0, 1e-9, 0, 0, 0)))
  mdl2$set_fit_options(scale_method = "none", warn_ca = FALSE,
                       svdtest_tol = 1e-6)
  mdl2$set_solve_options(report = "none")
  expect_output( # we expect output from the svd analysis
    expect_warning(mdl2$solve(), "Simulation not possible"))
  mdl2$set_fit_options(scale_method = "row")
  expect_silent(mdl2$solve())
  expect_identical(mdl2$get_solve_status(), "OK")
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit, tolerance = 1e-6)
})

test_that("row with only zeros", {
  mdl2 <- mdl$copy()
  mdl2$set_fit_options(scale_method = "row", svdtest_tol = 1e-6)
  mdl2$set_param(list(r2 = c(0, 0, 0, 0, 0, 0)))
  expect_output(
    expect_warning(
      mdl2$solve(options = list(report = "none")),
      "Simulation not possible"),
    NA  # we do not expect svd-analysis, because some rows are exactly zero
  )
})
