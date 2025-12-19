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

  mdl$fill_mdl_data(period = period, report = report, include_frmls = TRUE)

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
    mdl$fill_mdl_data(period = this_period, report = report, include_frmls = TRUE)
  }
  mdl_solved_data <- mdl$get_data()
  # Create summary based on report type
  if (report != "no") {
    dif <- tsdif(mdl_original_data, mdl_solved_data, fun = cvgdif, tol = 1e-4)

    if (length(dif$difnames) > 0) {
      overview <- dif$dif_table |>
        rename(initial = .data$value1,
               solved = .data$value2)

      n_solved <- sum(is.na(overview$initial) & !is.na(overview$solved))

      if (report == "period") {
        cat("\n===============================================\n")
        cat("Summary of solved variables:\n")
        cat("===============================================\n")
        print(overview)
        cat("\nTotal newly solved values: ", n_solved, "\n")
      } else if (report == "minimal") {
        cat("\nTotal newly solved values: ", n_solved, "\n")
      }
    } else {
      if (report %in% c("period", "minimal")) {
        cat("\nNo differences found between initial and solved data.\n")
      }
    }
  }

  return(invisible(mdl))
}
