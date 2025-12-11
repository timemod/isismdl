#' @import regts
#' @import nleqslv
#' @importFrom graphics plot
#' @importFrom dplyr group_split filter
#' @importFrom purrr map_dbl
#' @importFrom rlang set_names .data

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
    initial_guess, observed_data, dep_struc, report, ...) {

  # Check arguments ------------------------------------------------------------
  # TODO: controleer het soort model: het moet recursief zijn en geen lags bevatten.
  # TODO: check derived variables and observed variables are endogenous.
  if (length(solve_variables) != length(observed_variables)) {
    stop("Number of solve and observed variables are not equal")
  }
  if (report == "period") {
    cat("\n===============================================\n")
    cat("Solving solve variables for period", solve_period, "\n")
    cat(paste0("Solve variables: ", paste(solve_variables, collapse = ", "),
               "\n"))
    cat(paste0("Observed variables: ", paste(observed_variables, collapse = ", ")))
    cat("\n===============================================\n")
  }

  # Get de dependency structure -----------------------------------------------
  deps <- get_fit_deps(observed_variables, solve_variables,
                       solve_period = solve_period,  mdl = mdl,
                       dep_struc = dep_struc, observed_data = observed_data,
                       report = report)
  # TODO: make overview observed, byproduct and solve
  if (!any(deps$ok)) {
    stop("Some dependencies are not calculable")
  }

  active_equations <- deps[deps$calculable & deps$period == as.character(solve_period), ]$var
  mdl$set_eq_status(status = "inactive", pattern = "*")
  mdl$set_eq_status(status = "active", names = active_equations)
  mdl$order(silent = TRUE)
  if (report == "period") {
    cat(paste0("\nActive equations: ", paste(active_equations, collapse = ", "),
               "\n\n"))
  }

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
                         solve_period,  mdl, dep_struc, observed_data, report) {

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
  if (report == "period") {
    cat("\nfit dependencies:\n")
    print(deps)
  }

  deps
}
