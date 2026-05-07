library(testthat)
library(isismdl)
library(tibble)

rm(list = ls())
update_expected <- FALSE

simple_lag_model_text <- "
ident y1 = 0.8 * y1(-1) + 0.2 * z1;
ident y2 = 0.7 * y2(-1) + 0.3 * z2;
ident y3 = 0.6 * y3(-1) + 0.4 * z3;

ident w1 = 0.5 * y1 + 0.3 * z1;
ident w2 = 0.4 * y2 + 0.4 * z2;

ident obs1 = w1 + x1;
ident obs2 = w2 + x2;
ident obs3 = y3 + x3;
"

create_test_init_data <- function(period, var_names) {
  n_periods <- regts::nperiod(period)
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

  data <- regts::regts(data_matrix, names = var_names, period = period)
  return(data)
}


period <- as.period_range("2010/2015")
test_period <- "2011"

test_that("fill_mdl_data_solve basic functionality with missing period parameter", {

  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
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

  expect_equal(as.numeric(solved_data["2011", "y1"]), 2.8)
  expect_equal(as.numeric(solved_data["2011", "y2"]), 3.0)
  expect_equal(as.numeric(solved_data["2011", "y3"]), 2.0)

  # Check that if we put all variables in a single group, we get the same result.
  solve_df_single_group <- solve_df
  solve_df_single_group$group <- "SingleGroup"

  # Reset data before calling again
  mdl$init_data(data = data_init)

  mdl_solved2 <- mdl$fill_mdl_data_solve(
    solve_df = solve_df_single_group,
    report = "no"
  )
  expect_equal(solved_data, mdl_solved2$get_data())
})


test_that("fill_mdl_data_solve handles solve_df without group column", {

  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)

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

})

test_that("fill_mdl_data_solve handles solve_df without initial_guess column", {

  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)

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
  }, "Use only numerical or NA values in the initial_guess column.\nOffending values at rows: 1, 2, 3", fixed = TRUE)
})

test_that("fill_mdl_data_solve validates initial_guess values", {

  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)

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

})

test_that("fill_mdl_data_solve requires necessary columns in solve_df", {

  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)

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
  }, "Make sure the following column(s) exist in df: solve_period", fixed = TRUE)

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
  }, "Make sure the following column(s) exist in df: observed_variable", fixed = TRUE)

})

test_that("fill_mdl_data_solve handles duplicate groups", {

  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)

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

})

test_that("fill_mdl_data_solve with period parameter", {
  full_period <- as.period_range("2010/2015")
  solve_period <- as.period_range("2011")
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = full_period, silent = TRUE)

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

})

test_that("fill_mdl_data_solve with known output", {

  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)

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

})

test_that("fill_mdl_data_solve errors when model has feedback variables", {
  mdl_content <- c(
    "ident x = 0.5 * x + y;",
    "ident y = 0.5 * z;",
    "ident obs = x;"
  )

  mdl <- isismdl::isis_mdl(model_text = mdl_content, period = "2020", silent = TRUE)

  solve_df <- tribble(
    ~solve_period , ~group , ~observed_variable , ~solve_variable , ~initial_guess ,
    "2020"        , "A"    , "obs"              , "y"             , 0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(
      solve_df = solve_df,
      report = "no"
    ),
    "fill_mdl_data_solve does not support models with feedback variables.",
    fixed = TRUE
  )
})
test_that("fill_mdl_data_solve errors when solve variable is not NA", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  # Set y1 to not NA
  mdl$set_values(1.0, names = "y1", period = "2011")

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "y1",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no"),
      paste0(
        "Some solve variables are not NA. Non-NA values for the following variables and periods:\n",
        "  solve_variable solve_period\n",
        "1             y1         2011"
      ),
      fixed = TRUE
    )
  })

test_that("fill_mdl_data_solve reports multiple non-NA solve variables", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  # Set y1 and y2 to not NA
  mdl$set_values(1.0, names = "y1", period = "2011")
  mdl$set_values(2.0, names = "y2", period = "2012")

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    "2011",        "A",   "obs1",            "y1",            0.1,
    "2012",        "B",   "obs2",            "y2",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no"),
      paste0(
        "Some solve variables are not NA. Non-NA values for the following variables and periods:\n",
        "  solve_variable solve_period\n",
        "1             y1         2011\n",
        "2             y2         2012"
      ),
      fixed = TRUE
    )
  })

test_that("fill_mdl_data_solve errors when variables are not endogenous", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  # Solve variable is exogenous
  solve_df_exo_solve <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "obs1",            "x1",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(solve_df = solve_df_exo_solve, report = "no"),
    "The following solve variables are not endogenous model variables:\nx1.",
    fixed = TRUE
  )

  # Observed variable is exogenous
  solve_df_exo_obs <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,        "A",   "x1",            "y1",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(solve_df = solve_df_exo_obs, report = "no"),
    "The following observed variables are not endogenous model variables:\nx1.",
    fixed = TRUE
  )
})

test_that("fill_mdl_data_solve errors when model has leads", {
  mdl_content <- c(
    "ident x = 0.5 * x(1) + y;",
    "ident y = 0.5 * z;",
    "ident obs = x;"
  )

  mdl <- isismdl::isis_mdl(model_text = mdl_content, period = "2020/2021", silent = TRUE)

  solve_df <- tribble(
    ~solve_period , ~group , ~observed_variable , ~solve_variable , ~initial_guess ,
    "2020"        , "A"    , "obs"              , "y"             , 0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(
      solve_df = solve_df,
      report = "no"
    ),
    "fill_mdl_data_solve does not support models with leads.",
    fixed = TRUE
  )
})

test_that("fill_mdl_data_solve errors when solve_period is outside period argument", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  # period argument is 2011/2012, but solve_period is 2013
  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    "2011",        "A",   "obs1",            "y1",            0.1,
    "2013",        "B",   "obs2",            "y2",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(period = "2011/2012", solve_df = solve_df, report = "no"),
    "One or more solve periods in solve_df are outside the specified period range (2011/2012). Offending periods:\n 2013",
    fixed = TRUE
  )
})

test_that("fill_mdl_data_solve errors when observed variable is NA", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  # Set obs3 to NA
  mdl$set_values(NA, names = "obs3", period = "2011")

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    "2011",        "A",   "obs3",            "y3",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no"),
      paste0(
        "Some observed variables are NA. NA values for the following variables and periods:\n",
        "  observed_variable solve_period\n",
        "1              obs3         2011"
      ),
      fixed = TRUE
    )
  })

test_that("fill_mdl_data_solve errors when solve_df$observed_variable contains NA", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    "2011",        "A",   NA,                "y3",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no"),
    "The column 'observed_variable' in 'solve_df' contains one or more NA values.",
    fixed = TRUE
  )
})



test_that("fill_mdl_data_solve errors when solve_df contains duplicate solve variables", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    "2011",        "A",   "obs1",            "y1",            0.1,
    "2011",        "B",   "obs2",            "y1",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no"),
    "The following solve variables are specified more than once for the same period:\ny1[2011].",
    fixed = TRUE
  )
})

test_that("fill_mdl_data_solve errors when solve_df contains duplicate observed variables", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    "2011",        "A",   "obs1",            "y1",            0.1,
    "2011",        "B",   "obs1",            "y2",            0.1
  )

  expect_error(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no"),
    "The following observed variables are specified more than once for the same period:\nobs1[2011].",
    fixed = TRUE
  )
})

test_that("fill_mdl_data_solve: jacob is NULL when jacobian = FALSE", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,   "A",   "obs1",             "y1",            0.1
  )

  mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no", jacobian = FALSE)

  expect_null(mdl$get_jacobian())
})

test_that("fill_mdl_data_solve: jacob is a matrix with correct dims when jacobian = TRUE", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,   "A",   "obs1",             "y1",            0.1,
    test_period,   "A",   "obs2",             "y2",            0.1
  )

  mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no", jacobian = TRUE)


  jac <- mdl$get_jacobian()
  expect_true(is.matrix(jac))
  expect_equal(dim(jac), c(2L, 2L))
  expect_equal(rownames(jac), c("obs1", "obs2"))
  expect_equal(colnames(jac), c("y1", "y2"))
})

test_that("fill_mdl_data_solve: jacob reflects the last group solved", {
  # With two separate groups, jacob should come from the last group processed.
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,   "A",   "obs1",             "y1",            0.1,
    test_period,   "B",   "obs2",             "y2",            0.1
  )

  mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no", jacobian = TRUE)

  jac <- mdl$get_jacobian()
  expect_true(is.matrix(jac))
  # Last group is B: 1x1 jacobian with observed = obs2, solve = y2
  expect_equal(rownames(jac), "obs2")
  expect_equal(colnames(jac), "y2")
})

test_that("fill_mdl_data_solve: jacob is updated on repeated calls", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df1 <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,   "A",   "obs1",             "y1",            0.1
  )
  solve_df2 <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,   "A",   "obs2",             "y2",            0.1,
    test_period,   "A",   "obs3",             "y3",            0.1
  )

  mdl$init_data(data = data_init)
  mdl$fill_mdl_data_solve(solve_df = solve_df1, report = "no", jacobian = TRUE)
  jac1 <- mdl$get_jacobian()

  mdl$init_data(data = data_init)
  mdl$fill_mdl_data_solve(solve_df = solve_df2, report = "no", jacobian = TRUE)
  jac2 <- mdl$get_jacobian()

  expect_equal(dim(jac1), c(1L, 1L))
  expect_equal(dim(jac2), c(2L, 2L))
  expect_false(identical(jac1, jac2))
})

test_that("fill_mdl_data_solve: print_jacobian prints when TRUE, silent when FALSE", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,   "A",   "obs1",             "y1",            0.1
  )

  # With jacobian = TRUE and print_jacobian = TRUE, output should contain Jacobian message.
  # Note: report must be "period" for Jacobian to be printed.
  output_printed <- capture.output(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "period",
                             jacobian = TRUE, print_jacobian = TRUE)
  )
  expect_true(any(grepl("Final estimated jacobian matrix:", output_printed)))

  # With print_jacobian = FALSE, no jacobian output
  mdl$init_data(data = data_init)
  output_silent <- capture.output(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "period",
                             jacobian = TRUE, print_jacobian = FALSE)
  )
  expect_false(any(grepl("Final estimated jacobian matrix:", output_silent)))
})

test_that("fill_mdl_data_solve returns jacobian and updates status on failure", {
  # Use a non-solvable model to force failure (y1 = exp(y2), target y1 = -1)
  model_text <- "
  ident y1 = exp(y2);
  ident y2 = y2[-1];
  ident obs1 = y1;
  "
  mdl <- isis_mdl(model_text = model_text, period = "2019/2020", silent = TRUE)
  mdl$set_values(-1, names = "obs1", period = "2020")
  mdl$set_values(NA, names = "y2", period = "2019/2020")

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    "2020",   "A",   "obs1",             "y2",            0.1
  )

  expect_warning(
    mdl$fill_mdl_data_solve(solve_df = solve_df, report = "no", jacobian = TRUE),
    "Failed to solve the starting value with nleqslv"
  )

  expect_equal(mdl$get_solve_status(), "Simulation stopped")
  expect_false(is.null(mdl$get_jacobian()))
  expect_true(is.matrix(mdl$get_jacobian()))
})

test_that("fill_mdl_data_solve reporting output with report='minimal'", {
  mdl <- isis_mdl(model_text = simple_lag_model_text, period = period, silent = TRUE)
  var_names <- mdl$get_var_names()
  data_init <- create_test_init_data(period, var_names)
  mdl$init_data(data = data_init)

  solve_df <- tribble(
    ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
    test_period,   "A",   "obs1",             "y1",            0.1,
    test_period,   "B",   "obs2",             "y2",            0.1,
    test_period,   "C",   "obs3",             "y3",            0.1
  )

  # Calculate expected numbers based on the model and data_init
  # In 2011 (test_period):
  # obs1, obs2, obs3 are known (from create_test_init_data)
  # y1, y2, y3 are solve_variables (solved = 3)
  # w1, w2 are identities that depend on y1, y2 (filled post-solve)
  # Pre-solve: fill_mdl_data might fill some other variables if any (but here likely 0)

  expected_text <- paste0(
    "\n",
    "===============================================\n",
    "Summary of missing values replaced:\n",
    "===============================================\n\n",
    "by fill_mdl_data pre solve        : 0\n",
    "solved                            : 3\n",
    "by fill_mdl_data_solve post solve : 22\n"
  )

  expect_output(
    mdl$fill_mdl_data_solve(
      solve_df = solve_df,
      report = "minimal"
    ),
    regexp = expected_text,
    fixed = TRUE
  )
})
