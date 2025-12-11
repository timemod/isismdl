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
    mdl, period, fit_tbl, report, ...) {

  ensure_fit_tbl_cols <- function(fit_tbl, default_initial_guess = 0.1) {
    required_cols <- c("solve_period", "observed_variable", "solve_variable")
    missing_cols <- setdiff(required_cols, colnames(fit_tbl))
    if (length(missing_cols) > 0) {
      stop(
        "Make sure the following column(s) exist in fit_tbl: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    if (!("group" %in% colnames(fit_tbl))) {
      fit_tbl$group <- paste0("group_", seq_len(nrow(fit_tbl)))
      message(
        "There were no groups in the given tibble,\n",
        "so each solve and derived variable are treated separately"
      )
    }
    if (!("initial_guess" %in% colnames(fit_tbl))) {
      fit_tbl$initial_guess <- rep(default_initial_guess, nrow(fit_tbl))
      message(
        "No initial_guess is given, so for each entry the initial guess is ",
        default_initial_guess
      )
      return(fit_tbl)
    }

    ig <- fit_tbl$initial_guess
    if (!all(is.numeric(ig) | is.na(ig))) {
      stop(
        "Use only numerical or NA values in the initial_guess column.\n",
        "Offending values at rows: ",
        paste(which(!(is.numeric(ig) | is.na(ig))), collapse = ", ")
      )
    }

    ig_clean <- ifelse(is.na(ig) | ig == 0, default_initial_guess, ig)
    fit_tbl$initial_guess <- ig_clean
    return(fit_tbl)
  }

  fit_tbl <- ensure_fit_tbl_cols(fit_tbl)

  dep_struc <- mdl$get_dep_struct(one_lag_per_row = TRUE)

  if (missing(period)) {
    period <- mdl$get_data_period()
  } else {
    period <- as.period_range(period)
  }

  mdl$fill_mdl_data(period = period, report = report, include_frmls = TRUE)

  for (single_solve_period in fit_tbl |> group_split(.data$solve_period)) {
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
        report = report
      )
    }
    mdl$order(silent = TRUE)
    mdl$fill_mdl_data(period = this_period, report = report, include_frmls = TRUE)
  }
  # mdl$fill_mdl_data() period = looped period with report = "period", include_frmls = TRUE
  # if minimal: hvlheid NA before after door before files, after of solved soort summary
  #     no: niks afdrukken, ook niet dependencies enzo, gwn geen output. geen error is goed
  #     period: wel print(deps)
  return(mdl)
}
