library(isismdl)
library(testthat)

rm(list = ls())

context("fit square mdl singular")

mdl <- isis_mdl("mdl/square.mdl", period = 1000, silent = TRUE)

fit <- regts(matrix(c(0.412, 0.202, 0.813, 0.642, 0.123, 0.245), nrow = 1),
             period = 1000, names = paste0("w", 1:6))

rms <- rep(1, 6)
names(rms) <- paste0("x", 1:6)

mdl$set_fit(fit)
mdl$set_rms(rms)
mdl$set_fit_options(svdtest_tol = 1e-6)

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
  expect_silent(mdl2$solve(options = list(report = "none"),
                           fit_options = list(svdtest_tol = 1e-6)))
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit)

  mdl2 <- mdl_singular$copy()
  expect_silent(mdl2$solve(options = list(report = "none"),
                fit_options = list(svdtest_tol = 1e-10)))
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit)

  mdl2 <- mdl_singular$copy()
  mdl2$set_fit_options(svdtest_tol = 1e-6)
  expect_silent(mdl2$solve(options = list(report = "none")))
})

test_that("test2 (severer singular case, solution not possible)", {

  mdl_singular <- mdl$copy()
  params <- mdl_singular$get_param(names = c("r3", "r5"))
  params$r5 <- params$r3 + 1e-12 * params$r5
  mdl_singular$set_param(params)

  mdl2 <- mdl_singular$copy()
  expect_warning(mdl2$solve(options = list(report = "none"),
                           fit_options = list(svdtest_tol = 1e-6)))
  expect_identical(mdl2$get_solve_status(), "Simulation not possible")
  expect_false(isTRUE(all.equal(mdl2$get_data(pattern = "^w\\d"), fit)))

  # TODO: isismdl option nochkjac implementeren
})
