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
  dep_struc <- mdl$get_dep_struct(one_lag_per_row = TRUE)

  # function to check (required) columns in fit_tbl and add some if needed
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
      message("There were no groups in the given tibble,
              so each solve and derived variable are treated separately")
    }
    if (!("initial_guess" %in% colnames(fit_tbl))) {
      fit_tbl$initial_guess <- rep(as.numeric(default_initial_guess), nrow(fit_tbl))
      message(
        "No initial_guess is given, so for each entry the initial guess is ",
        default_initial_guess
      )
    }
    return(fit_tbl)
  }
  fit_tbl <- ensure_fit_tbl_cols(fit_tbl)

  if (missing(period)) {
    period <- mdl$get_data_period()
  } else {
    period <- as.period_range(period)
  }

  mdl$fill_mdl_data(period = period, report = report, include_frmls = TRUE)

  # TODO: observed_data to available_data
  observed_data <- get_observed_data(mdl$get_data())

  # TODO: nested loop, first loop through period (so more than 1 period possible)
  for (single_solve_period in fit_tbl |> group_split(.data$solve_period)) {
    this_period <- single_solve_period$solve_period[1]
    for (group_data in single_solve_period |> group_split(.data$group)) {
      # Following codeblock makes a numerical named list of initial_guess
      # which will be needed when calling nleqslv
      initial_guess_num <- set_names(group_data$initial_guess, group_data$solve_variable) |>
        map_dbl(~ {
          x <- .x
          if (is.null(x) || length(x) == 0 || is.na(x) || identical(x, "") || x == 0) return(0.1)
          num <- suppressWarnings(as.numeric(x))
          if (is.na(num)) {
            stop(
              "Please do not enter white spaces or text in `initial_guess`. ",
              "Keep the type consistent: either all numeric values or
              numeric values between quotes and empty strings ('')."
            )
          }
          num
        })
      # function solve_single_group is in fmds_single.R
      solve_single_group(
        mdl,
        solve_period = this_period,
        solve_variables = group_data$solve_variable,
        observed_variables = group_data$observed_variable,
        observed_data = observed_data,
        dep_struc = dep_struc,
        initial_guess = initial_guess_num
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
