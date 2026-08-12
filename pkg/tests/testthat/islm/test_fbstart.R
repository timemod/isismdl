library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

mdl <- read_mdl("islm_model.ismdl", silent = TRUE)$solve(options = list(report = "none"))
sol <- mdl$get_data()

test_that("r invalid at current period", {
  mdl2 <- mdl$copy()

  mdl2$set_values(NA, names = "r", period = "2015q2")

  expect_warning(
    expect_output(
      mdl2$solve(),
      "1 missing or invalid initial feedback values"
    ),
    "Simulation not possible",
    fixed = TRUE
  )
  expect_equal(mdl2$get_solve_status(), "Simulation not possible")

  mdl2_previous <- mdl2$copy()
  expect_output(
    mdl2_previous$solve(options = list(fbstart = "previous")),
    "Convergence for 2016Q3"
  )
  expect_equal(mdl2_previous$get_solve_status(), "OK")
  expect_equal(mdl2_previous$get_data(), sol)

  mdl2_curifok <- mdl2$copy()
  expect_output(
    mdl2_curifok$solve(options = list(fbstart = "previous")),
    "Convergence for 2016Q3"
  )
  expect_equal(mdl2_curifok$get_solve_status(), "OK")
  expect_equal(mdl2_curifok$get_data(), sol)

  mdl2_previfok <- mdl2$copy()
  expect_output(
    mdl2_previfok$solve(options = list(fbstart = "previous")),
    "Convergence for 2016Q3"
  )
  expect_equal(mdl2_previfok$get_solve_status(), "OK")
  expect_equal(mdl2_previfok$get_data(), sol)
})


mdl2 <- mdl$copy()
expect_warning(
  mdl2$solve(options = list(fbstart = "previfok", maxiter = 0, report = "none")),
  "Simulation stopped"
)

mdl2 <- mdl$copy()
mdl2$solve(options = list(fbstart = "curifok", maxiter = 0, report = "none"))
expect_equal(mdl$get_solve_status(), "OK")
expect_equal(mdl$get_data(), sol)
