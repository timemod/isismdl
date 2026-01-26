library(testthat)
library(isismdl)
library(tibble)

rm(list = ls())

test_that("solve_exo_internal solves exogenous variables for a simple model", {
  mdl_file <- tempfile(fileext = ".mdl")
  writeLines(c(
    "ident x1 = 0.5 * x1(-1) + 0.3 * y1;",
    "ident x2 = 0.6 * x2(-1) + 0.4 * y2;",
    "ident x3 = 0.4 * x3(-1) + 0.2 * y3;",
    "ident obs1 = 2.0 * x1;",
    "ident obs2 = 1.5 * x2;",
    "ident obs3 = 1.2 * x3;"
  ), mdl_file)

  mdl <- isismdl::isis_mdl(mdl_file, period = "2019/2020", silent = TRUE)

  mdl$set_values(10, names = "x1", period = "2019")
  mdl$set_values(20, names = "x2", period = "2019")
  mdl$set_values(30, names = "x3", period = "2019")

  mdl$set_values(50, names = "obs1", period = "2020")
  mdl$set_values(60, names = "obs2", period = "2020")
  mdl$set_values(70, names = "obs3", period = "2020")

  target_obs <- c(50, 60, 70)

  solve_period <- "2020"

  exo_vars <- c("y1", "y2", "y3")
  target_vars <- c("obs1", "obs2", "obs3")

  res <- mdl$solve_exo(
    solve_period = solve_period,
    exo_vars     = exo_vars,
    target_vars  = target_vars,
    report       = "no",
    jacobian     = FALSE
  )

  mdl$run_eqn(period = "2020", solve_order = TRUE)

  model_obs <- mdl$get_data(
    names  = c("obs1", "obs2", "obs3"),
    period = "2020"
  )

  expect_true(all(abs(as.numeric(model_obs) - target_obs) < 1e-6))

  solved_y <- mdl$get_data(names = exo_vars, period = "2020")
  expect_false(any(is.na(solved_y)))
  expect_true(all(is.finite(solved_y)))

  unlink(mdl_file)
})

test_that("solve_exo_internal errors when target variables contain NA", {
  mdl_file <- tempfile(fileext = ".mdl")
  writeLines(c(
    "ident x1 = 0.5 * x1(-1) + 0.3 * y1;",
    "ident obs1 = 2.0 * x1;"
  ), mdl_file)

  mdl <- isismdl::isis_mdl(mdl_file, period = "2019/2020", silent = TRUE)

  mdl$set_values(10, names = "x1", period = "2019")

  expect_error(
    mdl$solve_exo(
      solve_period = "2020",
      exo_vars     = "y1",
      target_vars  = "obs1",
      report       = "no"
    ),
    regexp = "target variables contain NA values"
  )

  unlink(mdl_file)
})

test_that("solve_exo solves for a period range", {
  mdl_file <- tempfile(fileext = ".mdl")
  writeLines(c(
    "ident x = y;",
    "ident obs = 2 * x;"
  ), mdl_file)

  mdl <- isismdl::isis_mdl(mdl_file, period = "2020Q1/2020Q2", silent = TRUE)

  # Set target values for two quarters
  mdl$set_values(c(100, 200), names = "obs", period = "2020Q1/2020Q2")

  # Solve for the range
  mdl$solve_exo(
    solve_period = "2020Q1/2020Q2",
    exo_vars     = "y",
    target_vars  = "obs",
    report       = "no"
  )

  # Verify values in both quarters
  data <- mdl$get_data(names = c("y", "obs"), period = "2020Q1/2020Q2")
  expect_equal(as.numeric(data[, "y"]), c(50, 100))
  expect_equal(as.numeric(data[, "obs"]), c(100, 200))

  unlink(mdl_file)
})
