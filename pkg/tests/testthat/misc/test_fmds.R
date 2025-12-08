library(testthat)
library(dplyr)
library(tibble)
library(regts)
library(nleqslv)
library(purrr)
library(rlang)
library(graphics)
library(igraph)
library(tidyr)
library(stringr)

rm(list = ls())
update_expected <- FALSE

create_simple_lag_model <- function() {
  mdl_content <- "
ident x1 = 0.5 * x1(-1) + 0.3 * y1;
ident x2 = 0.6 * x2(-1) + 0.4 * y2;
ident x3 = 0.4 * x3(-1) + 0.2 * y3;
ident obs1 = 2.0 * x1;
ident obs2 = 1.5 * x2;
ident obs3 = 1.2 * x3;
"

  mdl_file <- tempfile("test_fmds_", fileext = ".mdl")
  writeLines(mdl_content, mdl_file)
  return(mdl_file)
}

create_test_init_data <- function(period, var_names) {
  n_periods <- nperiod(period)
  n_vars <- length(var_names)

  # Create initial values - simple sequences for each variable
  data_matrix <- matrix(0, nrow = n_periods, ncol = n_vars)

  # Set different initial values for different variables
  for (i in seq_along(var_names)) {
    if (grepl("^x", var_names[i])) {
      data_matrix[, i] <- seq(10, 10 + n_periods - 1)
    } else if (grepl("^y", var_names[i])) {
      data_matrix[, i] <- seq(5, 5 + n_periods - 1)
    } else if (grepl("^obs", var_names[i])) {
      data_matrix[, i] <- NA  # Will be calculated
    } else {
      data_matrix[, i] <- seq(1, n_periods)
    }
  }

  data <- regts(data_matrix, names = var_names, period = period)
  return(data)
}


period <- as.period_range("2010/2015")
test_period <- "2011"

test_that("fill_mdl_data_solve basic functionality with missing period parameter", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)


  fit_tbl <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            0.1,
    test_period,        "B",   "obs2",            "y2",            0.1,
    test_period,        "C",   "obs3",            "y3",            0.1,
  )


  expect_true("fill_mdl_data_solve" %in% names(mdl))

  mdl_solved <- mdl$fill_mdl_data_solve(
    fit_tbl = fit_tbl,
    report = "no"
  )


  expect_s3_class(mdl_solved, "IsisMdl")

  solved_data <- mdl_solved$get_data()
  expect_false(is.null(solved_data))

  unlink(mdl_file)
})


test_that("fill_mdl_data_solve handles fit_tbl without group column", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  fit_tbl <- tribble(
    ~solve_period, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "obs1",            "y1",            0.1,
    test_period,        "obs2",            "y2",            0.1,
    test_period,        "obs3",            "y3",            0.1
  )

  expect_message({
    mdl_solved <- mdl$fill_mdl_data_solve(
      fit_tbl = fit_tbl,
      report = "no"
    )
  }, "There were no groups")

  unlink(mdl_file)
})

test_that("fill_mdl_data_solve handles fit_tbl without initial_guess column", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  fit_tbl <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable,
    test_period,        "A",   "obs1",            "y1",
    test_period,        "B",   "obs2",            "y2",
    test_period,        "C",   "obs3",            "y3"
  )

  expect_message({
    mdl_solved <- mdl$fill_mdl_data_solve(
      fit_tbl = fit_tbl,
      report = "no"
    )
  }, "No initial_guess is given")

  unlink(mdl_file)
})

test_that("fill_mdl_data_solve validates initial_guess values", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  fit_tbl <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            "",
    test_period,        "B",   "obs2",            "y2",            "0.5",
    test_period,        "C",   "obs3",            "y3",            "0"
  )

  expect_no_error({
    mdl_solved <- mdl$fill_mdl_data_solve(
      fit_tbl = fit_tbl,
      report = "no"
    )
  })

  fit_tbl2 <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period, "A", "obs1", "y1", 0.5
  )

  expect_no_error({
    mdl_solved <- mdl$fill_mdl_data_solve(
      fit_tbl = fit_tbl2,
      report = "no"
    )
  })

  # Test with invalid text (should error)
  fit_tbl3 <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            " ",
    test_period,        "B",   "obs2",            "y2",            "0.5",
    test_period,        "C",   "obs3",            "y3",            "invalid"
  )

  expect_error({
    mdl_solved <- mdl$fill_mdl_data_solve(
      fit_tbl = fit_tbl3,
      report = "no"
    )
  }, "Please do not enter white spaces or text")

  unlink(mdl_file)
})

test_that("fill_mdl_data_solve requires necessary columns in fit_tbl", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  # Missing solve_period
  fit_tbl_bad1 <- tribble(
    ~observed_variable, ~solve_variable,
    "obs1", "y1"
  )

  expect_error({
    mdl$fill_mdl_data_solve(
      fit_tbl = fit_tbl_bad1,
      report = "no"
    )
  }, "Make sure the following column\\(s\\) exist")

  # Missing observed_variable
  fit_tbl_bad2 <- tribble(
    ~solve_period, ~solve_variable,
    test_period, "y1"
  )

  expect_error({
    mdl$fill_mdl_data_solve(
      fit_tbl = fit_tbl_bad2,
      report = "no"
    )
  }, "Make sure the following column\\(s\\) exist")

  unlink(mdl_file)
})

test_that("fill_mdl_data_solve handles duplicate groups", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)


  fit_tbl <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            0.1,
    test_period,        "B",   "obs2",            "y2",            0.1,
    test_period,        "B",   "obs3",            "y3",            0.1,
  )

  expect_no_error({
    mdl_solved <- mdl$fill_mdl_data_solve(
      fit_tbl = fit_tbl,
      report = "no"
    )
  })

  unlink(mdl_file)
})

test_that("fill_mdl_data_solve with period parameter", {

  mdl_file <- create_simple_lag_model()
  full_period <- as.period_range("2010/2015")
  solve_period <- as.period_range("2011/2013")

  mdl <- isis_mdl(mdl_file, full_period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(full_period, var_names)
  mdl$init_data(data = data_init)

  fit_tbl <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,     "A",              "obs1",            "y1",           0.1
  )

  mdl_solved <- mdl$fill_mdl_data_solve(
    period = solve_period,
    fit_tbl = fit_tbl,
    report = "no"
  )

  expect_s3_class(mdl_solved, "IsisMdl")
  expect_false(is.null(mdl_solved$get_data()))

  unlink(mdl_file)
})

test_that("fill_mdl_data_solve with known output", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  fit_tbl <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,      "A",             "obs1",            "y1",       0.1,
    test_period,      "B",             "obs2",            "y2",       0.1
  )

  mdl_solved <- mdl$fill_mdl_data_solve(
    fit_tbl = fit_tbl,
    report = "no"
  )

  solved_data <- mdl_solved$get_data()

  expected_file <- "expected_output/fmds_simple.rds"
  if (update_expected) {
    dir.create("expected_output", showWarnings = FALSE, recursive = TRUE)
    saveRDS(solved_data, file = expected_file)
  }

  if (file.exists(expected_file)) {
    expect_known_value(solved_data,
                       file = expected_file,
                       update = update_expected)
  }

  unlink(mdl_file)
})
