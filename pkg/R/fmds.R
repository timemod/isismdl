#' fmds = fill_model_data_solve internal
#' @import regts
#' @import nleqslv
#' @importFrom graphics plot
#' @importFrom dplyr group_split filter inner_join select
#' @importFrom purrr map_dbl
#' @importFrom rlang set_names .data
#' @noRd

# This function solves multiple groups of derived variables with
# the accompanying observed variables.

fmds <- function(
    mdl, period, solve_df, report, default_initial_guess = 0.1, jacobian,
    ...) {

  solve_df <- ensure_solve_df_cols(solve_df,
                                   default_initial_guess = default_initial_guess)

  # Check that solve_variables and observed_variables are endogenous.
  all_endo <- mdl$get_endo_names()
  missing_v <- setdiff(solve_df$solve_variable, all_endo)
  if (length(missing_v) > 0) {
    stop(
      "The following solve variables are not endogenous variables of the model: ",
      paste(missing_v, collapse = ", ")
    )
  }
  missing_o <- setdiff(solve_df$observed_variable, all_endo)
  if (length(missing_o) > 0) {
    stop(
      "The following observed variables are not endogenous variables of the model: ",
      paste(missing_o, collapse = ", ")
    )
  }

  # Check that each equation name is the same as the name of the corresponding
  # endogenous variable (LHS).
  # This IS the case for the current implementation of fmds.
  eq_names <- mdl$get_eq_names()
  if (!all(all_endo %in% eq_names)) {
    stop(paste(
      "Currently, fill_mdl_data_solve requires that all endogenous",
      "variables have an equation with the same name."
    ))
  }

  # Check that solve_variables are NA at their respective solve_periods.
  mdl_data_long <- regts::as_data_frame(mdl$get_data(), format = "long",
                                        name_col = "solve_variable",
                                        period_col = "solve_period") |>
    dplyr::filter(!is.na(.data$value))

  offending_values <- solve_df |>
    dplyr::inner_join(mdl_data_long, by = c("solve_period", "solve_variable")) |>
    dplyr::select("solve_variable", "solve_period")

  if (nrow(offending_values) > 0) {
    cat("\nNon-NA values for the following variables and periods:\n")
    print(as.data.frame(offending_values))
    stop("Some solve variables are not NA: See the table above.")
  }

  # Check the model type: it must be recursive (no feedback variables and no leads).
  if (length(mdl$get_endo_names(type = "feedback")) > 0) {
    stop("fill_mdl_data_solve does not support models with feedback variables.")
  }
  if (mdl$get_maxlead() > 0) {
    stop("fill_mdl_data_solve does not support models with leads.")
  }

  mdl_original_data <- mdl$get_data()
  dep_struc <- mdl$get_dep_struct(one_lag_per_row = TRUE)

  if (missing(period)) {
    period <- mdl$get_data_period()
  } else {
    period <- as.period_range(period)
  }

  mdl$fill_mdl_data(period = period, report = "no", include_frmls = TRUE)

  for (single_solve_period in solve_df |> group_split(.data$solve_period)) {
    this_period <- single_solve_period$solve_period[1]
    for (group_data in single_solve_period |> group_split(.data$group)) {

      observed_data <- get_observed_data(mdl$get_data())

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
        jacobian = jacobian,
        ...
      )
    }

    # Fill model data for this period.
    mdl$fill_mdl_data(period = this_period, report = "no", include_frmls = TRUE)
  }

  # Finally solve for missing data for the full period again, more variables
  # may be calculated.
  mdl$fill_mdl_data(period = period, report = "no", include_frmls = TRUE)

  mdl_solved_data <- mdl$get_data()

  dif_table <- tsdif(mdl_original_data, mdl_solved_data, fun = cvgdif,
                     tol = 1e-4)$dif_table

  # Check that only NA values have been modified.
  if (!is.null(dif_table) && !all(is.na(dif_table$value1))) {
    stop(paste(
      "One or more valid (non-NA) values have been modified during the solve",
      "process. This is not allowed. Offending variables:",
      paste(unique(dif_table$name[!is.na(dif_table$value1)]), collapse = ", ")
    ))
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
        cat("===============================================\n\n")
        print(overview)
      }
      cat("\nTotal number of replaced missing/invalid values: ", n_filled, "\n")
    } else {
      cat("\nNo missing/invalid values have been replaced.\n")
    }
  }

  return(invisible(mdl))
}
