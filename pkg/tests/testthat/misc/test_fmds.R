library(testthat)
library(isismdl)
library(tibble)

rm(list = ls())
update_expected <- FALSE

create_simple_lag_model <- function() {
  mdl_content <- "
ident y1 = 0.8 * y1(-1) + 0.2 * z1;
ident y2 = 0.7 * y2(-1) + 0.3 * z2;
ident y3 = 0.6 * y3(-1) + 0.4 * z3;

ident w1 = 0.5 * y1 + 0.3 * z1;
ident w2 = 0.4 * y2 + 0.4 * z2;

ident obs1 = w1 + x1;
ident obs2 = w2 + x2;
ident obs3 = y3 + x3;
"

  mdl_file <- tempfile("test_fmds_", fileext = ".mdl")
  writeLines(mdl_content, mdl_file)
  return(mdl_file)
}

create_test_init_data <- function(period, var_names) {
  n_periods <- nperiod(period)
  n_vars <- length(var_names)

  # Create initial values - simple sequences for each variable
  data_matrix <- matrix(NA, nrow = n_periods, ncol = n_vars)

  # Set different initial values for different variables
  for (i in seq_along(var_names)) {
    if (grepl("^x", var_names[i])) {
      data_matrix[, i] <- seq(10, 10 + n_periods - 1)
    } else if (grepl("^z", var_names[i])) {
      data_matrix[, i] <- seq(1, n_periods)
    } else if (grepl("^obs", var_names[i])) {
      data_matrix[, i] <- seq(12, 12 + n_periods - 1)
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


  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            0.1,
    test_period,        "B",   "obs2",            "y2",            0.1,
    test_period,        "C",   "obs3",            "y3",            0.1,
  )


  expect_true("fill_mdl_data_solve" %in% names(mdl))

  mdl_solved <- mdl$fill_mdl_data_solve(
    solve_df = solve_df,
    report = "no"
  )

  expect_s3_class(mdl_solved, "IsisMdl")

  solved_data <- mdl_solved$get_data()
  expect_false(is.null(solved_data))

  unlink(mdl_file)
})


test_that("fill_mdl_data_solve handles solve_df without group column", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "obs1",            "y1",            0.1,
    test_period,        "obs2",            "y2",            0.1,
    test_period,        "obs3",            "y3",            0.1
  )

  expect_message({
    mdl_solved <- mdl$fill_mdl_data_solve(
      solve_df = solve_df,
      report = "no"
    )
  }, "There were no groups")

  unlink(mdl_file)
})

test_that("fill_mdl_data_solve handles solve_df without initial_guess column", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable,
    test_period,        "A",   "obs1",            "y1",
    test_period,        "B",   "obs2",            "y2",
    test_period,        "C",   "obs3",            "y3"
  )

  expect_message({
    mdl_solved <- mdl$fill_mdl_data_solve(
      solve_df = solve_df,
      report = "no"
    )
  }, "No initial_guess is given")

  # Test with invalid initial_guess (should error)
  solve_df3 <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            " ",
    test_period,        "B",   "obs2",            "y2",            "0.5",
    test_period,        "C",   "obs3",            "y3",            "invalid"
  )

  expect_error({
    mdl_solved <- mdl$fill_mdl_data_solve(
      solve_df = solve_df3,
      report = "no"
    )
  }, "Use only numerical or NA values")
  unlink(mdl_file)
})

test_that("fill_mdl_data_solve validates initial_guess values", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            0,
    test_period,        "B",   "obs2",            "y2",            NA,
    test_period,        "C",   "obs3",            "y3",            NA
  )

  expect_no_error({
    mdl_solved <- mdl$fill_mdl_data_solve(
      solve_df = solve_df,
      report = "no"
    )
  })

  unlink(mdl_file)

})

test_that("fill_mdl_data_solve requires necessary columns in solve_df", {

  mdl_file <- create_simple_lag_model()
  mdl <- isis_mdl(mdl_file, period, silent = TRUE)

  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  # Missing solve_period
  solve_df_bad1 <- tribble(
    ~observed_variable, ~solve_variable,
    "obs1", "y1"
  )

  expect_error({
    mdl$fill_mdl_data_solve(
      solve_df = solve_df_bad1,
      report = "no"
    )
  }, "Make sure the following column\\(s\\) exist")

  # Missing observed_variable
  solve_df_bad2 <- tribble(
    ~solve_period, ~solve_variable,
    test_period, "y1"
  )

  expect_error({
    mdl$fill_mdl_data_solve(
      solve_df = solve_df_bad2,
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


  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            0.1,
    test_period,        "B",   "obs2",            "y2",            0.1,
    test_period,        "B",   "obs3",            "y3",            0.1,
  )

  expect_no_error({
    mdl_solved <- mdl$fill_mdl_data_solve(
      solve_df = solve_df,
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

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,     "A",              "obs1",            "y1",           0.1
  )

  mdl_solved <- mdl$fill_mdl_data_solve(
    period = solve_period,
    solve_df = solve_df,
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

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,      "A",             "obs1",            "y1",       0.1,
    test_period,      "B",             "obs2",            "y2",       0.1
  )

  mdl_solved <- mdl$fill_mdl_data_solve(
    solve_df = solve_df,
    report = "no"
  )

  solved_data <- mdl_solved$get_data()

  expect_known_value(
    solved_data,
    "expected_output/fmds_simple.rds"
  )

  unlink(mdl_file)
})
