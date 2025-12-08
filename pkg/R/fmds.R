# fmds = fill_model_data_solve internal
#' @import regts
#' @import nleqslv
#' @importFrom graphics plot
#' @importFrom dplyr group_split filter
#' @importFrom purrr map_dbl
#' @importFrom rlang set_names
#' @importFrom magrittr %>%

# TODO: Currently, the derived and observed value are for the same year, but is
# could be generalized to the situation where the period of the derived variable
# is different from that of the observed variable.

# INPUT:
#    mdl
#       A reference to an IsisMdl object. NOTE: the IsisMdl object is modified
#       in place.
#    solve_period
#        A period object
#    solve_variables
#
#    observed_variables
#
#    initial_guess
#        A named numeric vector with initial guess of the derived variables.
#    observed_data
#    dep_struc
#
#
# Solve a single solve variable given an observed variable.
solve_single_group <- function(
    mdl, solve_period, solve_variables, observed_variables,
    initial_guess, observed_data, dep_struc, ...) {

  # Check arguments ------------------------------------------------------------
  # TODO: controleer het soort model: het moet recursief zijn en geen lags bevatten.
  # TODO: check derived variables and observed variables are endogenous.
  if (length(solve_variables) != length(observed_variables)) {
    stop("Number of solve and observed variables are not equal")
  }
  cat("\n===============================================\n")
  cat("Solving solve variables for period", solve_period, "\n")
  cat(paste0("Solve variables: ", paste(solve_variables, collapse = ", "),
             "\n"))
  cat(paste0("Observed variables: ", paste(observed_variables, collapse = ", ")))
  cat("\n===============================================\n")
  # Get de dependency structure -----------------------------------------------
  deps <- get_fit_deps(observed_variables, solve_variables,
                       solve_period = solve_period,  mdl = mdl,
                       dep_struc = dep_struc, observed_data = observed_data)
  # TODO: make overview observed, byproduct and solve
  if (!any(deps$ok)) {
    stop("Some dependencies are not calculable")
  }

  active_equations <- deps[deps$calculable & deps$period == as.character(solve_period), ]$var
  mdl$set_eq_status(status = "inactive", pattern = "*")
  mdl$set_eq_status(status = "active", names = active_equations)
  mdl$order(silent = TRUE)

  cat(paste0("\nActive equations: ", paste(active_equations, collapse = ", "),
             "\n\n"))


  observed_values <- mdl$get_data(names = observed_variables, period = solve_period) |>
    as.numeric()

  n <- length(solve_variables)
  # Verschil berekenen tussen observed en calculated observed value
  f_solve <- function(x) {
    data <- regts(matrix(x, ncol = n), names = solve_variables,
                  period = solve_period)
    mdl$set_data(data)
    mdl$solve(period = solve_period,
              options = list(erropt = "silent", report = "none"))
    retval <- observed_values -
      as.numeric(mdl$get_data(names = observed_variables, period = solve_period))
    return(retval)
  }

  ret <- nleqslv(initial_guess, fn = f_solve, ...)
  # TODO: check output van ret if it succeeded
  # cat("nleqslv\n")
  # print(ret)

  # TODO: error when solve failed.
  # And set derived variables to NA.

  mdl$set_eq_status(status = "active", pattern = "*")

  return(ret)
}

get_fit_deps <- function(observed_variable, derived_variable,
                         solve_period,  mdl, dep_struc, observed_data) {

  observed <- data.frame(var = observed_variable,
                         period = as.character(solve_period))

  derivable <- data.frame(var = derived_variable,
                          period = as.character(solve_period))

  dep_graph <- find_deps(
    final_destinations = observed,
    final_sources = derivable,
    dep_struc = dep_struc,
    observed_data = observed_data,
    mdl = mdl,
    ignore_observed = TRUE,
    stop_if_final_dest_observed = FALSE
  )

  plot(dep_graph)

  deps <- as_data_frame_deps(dep_graph) |>
    # derivable variables should not computed, they are based on the
    # initial value.
    dplyr::filter(!.data$is_final_src)

  # TODO: controleer dat alle variabelen in deps 'calculable' zijn.

  cat("\nfit dependencies:\n")
  print(deps)

  deps
}

# This function solves multiple groups of derived variables with the accompanying
# observed variables.
# TODO: draai fill_mdl_data eerst in functie voor period
# (defaults: include_frmls = TRUE, "minimal")
# TODO: add report parameter doorgeven aan mdl$fill_mdl_data()

fmds <- function(
    mdl, period, fit_tbl, report, ...) {
  dep_struc <- mdl$get_dep_struct(one_lag_per_row = TRUE)
  mdl <- mdl$copy()

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
  for (single_solve_period in fit_tbl %>% group_split(.data$solve_period)) {
    for (group_data in single_solve_period %>% group_split(.data$group)) {
      # Following codeblock makes a numerical named list of initial_guess
      # which will be needed when calling nleqslv
      initial_guess_num <- set_names(group_data$initial_guess, group_data$solve_variable) %>%
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

      this_period <- group_data$solve_period[1]

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
  }
  # fill again
  #
  # mdl$fill_mdl_data() period= looped period with report = "period", include_frmls = TRUE
  # if minimal: hvlheid NA before after door before filles, after of solved soort summary
  #     no: niks afdrukken, ook niet dependencies enzo, gwn geen output. geen error is goed
  #     period: wel print(deps)
  mdl$order()
  mdl$fill_mdl_data(period = period, report = report, include_frmls = TRUE)
  return(mdl)
}
