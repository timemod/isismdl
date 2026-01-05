# fmds = fill_model_data_solve internal
#' @import regts
#' @import nleqslv
#' @importFrom graphics plot
#' @importFrom dplyr group_split filter
#' @importFrom purrr map_dbl
#' @importFrom rlang set_names .data

# This function solves multiple groups of derived variables with
# the accompanying observed variables.

fmds <- function(
    mdl, period, solve_df, report, default_initial_guess = 0.1, ...) {

  solve_df <- ensure_solve_df_cols(solve_df,
                                   default_initial_guess = default_initial_guess)

  # TODO: check that solve_variables are NA.
  # The purpose of fill_mdl_data_solve is to replace NA values with non-NA values,
  # so the solve variable should not already have a value.

  # TODO: this function currently assumes that the equation names are the same
  # as the names of the left hand side equations (endogenous variables).
  # This is the case for the current Zoem model. Check that this is the case,
  # or generalize the function so that it also works with model with different
  # equation names.

  # TODO: controleer het soort model: het moet recursief zijn en geen lags bevatten.
  # TODO: check solved_variables and observed variables are endogenous.

  inactives <- mdl$get_endo_names(status = "inactive")
  on.exit({
    mdl$set_eq_status(status = "active", pattern = "*")
    mdl$set_eq_status(status = "inactive", names = inactives)
    mdl$order(silent = TRUE)
  }) # Restore original active and inactive equations
  mdl_original_data <- mdl$get_data()
  dep_struc <- mdl$get_dep_struct(one_lag_per_row = TRUE)

  if (missing(period)) {
    period <- mdl$get_data_period()
  } else {
    period <- as.period_range(period)
  }

  mdl$fill_mdl_data(period = period, report = "no", include_frmls = TRUE)

  for (single_solve_period in solve_df |> group_split(.data$solve_period)) {
    # TODO: observed_data to available_data
    observed_data <- get_observed_data(mdl$get_data())
    this_period <- single_solve_period$solve_period[1]
    for (group_data in single_solve_period |> group_split(.data$group)) {
      # numerical vector used in nleqslv
      initial_guess_vector <- group_data$initial_guess

      # function solve_single_group is in fmds_single.R
      solve_single_group(
        mdl,
        solve_period = this_period,
        solve_variables = group_data$solve_variable,
        observed_variables = group_data$observed_variable,
        observed_data = observed_data,
        dep_struc = dep_struc,
        initial_guess = initial_guess_vector,
        report = report,
        ...
      )
    }
    mdl$order(silent = TRUE)
    mdl$fill_mdl_data(period = this_period, report = "no", include_frmls = TRUE)
  }

  # Finally solve for missing data for the full period.
  mdl$fill_mdl_data(period = period, report = "no", include_frmls = TRUE)

  mdl_solved_data <- mdl$get_data()

  dif_table <- tsdif(mdl_original_data, mdl_solved_data, fun = cvgdif,
                     tol = 1e-4)$dif_table

  # Check that only NA values have been modified.
  if (!is.null(dif_table) && !all(is.na(dif_table$value1))) {
    # TODO: Improve error message
    stop("One or more valid values has been modified, that is not allowed")
  }

  # Create summary based on report type
  if (report != "no") {
    if (!is.null(dif_table)) {
      overview <- dif_table |>
        filter(!is.na(.data$value2)) |>
        select("name", "period", "value2") |>
        rename(replacement = .data$value2)

      n_filled <- nrow(overview)

      if (report == "period") {
        cat("\n===============================================\n")
        cat("Summary of replaced missing/invalid values:\n")
        cat("===============================================\n")
        print(overview)
      }
      cat("\nTotal number of replaced missing/invalid values: ", n_filled, "\n")
    } else {
      cat("\nNo missing/invalid values have been replaced.\n")
    }
  }

  return(invisible(mdl))
}
