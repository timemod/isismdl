#' Solve a single group of solve variables given observed variables
#'
#' @param mdl A reference to an `IsisMdl` object. NOTE: the `IsisMdl` object is
#'   modified in place.
#' @param solve_period A `regts` period object specifying the period for which to solve.
#' @param solve_variables A character vector with names of the variables to be solved.
#' @param observed_variables A character vector with names of the variables whose
#'   observed values must be matched.
#' @param initial_guess A named numeric vector with the initial guess for the
#'   `solve_variables`.
#' @param observed_data A data frame or tibble containing the observed values
#'   (used for finding dependencies).
#' @param dep_struc The model's dependency structure. This can be either a data frame
#'   (returned by `mdl$get_dep_struct()`) or a pre-processed list
#'   (returned by `dep_struct_to_list()`).
#' @param report String ('no', 'period', or 'all') describing the level of reporting.
#' @param jacobian Logical. Whether to calculate and use the Jacobian.
#' @param ... Extra arguments passed to `nleqslv`.
#'
#' @import regts
#' @import nleqslv
#' @importFrom graphics plot
#' @importFrom dplyr group_split filter
#' @importFrom purrr map_dbl
#' @importFrom rlang set_names .data
#' @noRd
fmds_group <- function(
    mdl, solve_period, solve_variables, observed_variables,
    initial_guess, observed_data, dep_struc, report, jacobian,
    ...) {

  # Check arguments ------------------------------------------------------------

  if (report == "period") {
    cat("\n===============================================\n")
    cat("Solving solve variables for period ", solve_period, "\n", sep = "")
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

  equations <- deps[deps$calculable & deps$period == as.character(solve_period), ]$var
  if (report == "period") {
    cat(paste0("\nEquations to run: ", paste(equations, collapse = ", "),
               "\n\n"))
  }

  observed_values <- mdl$get_data(names = observed_variables, period = solve_period) |>
    as.numeric()

  if (anyNA(observed_variables)) {
    stop("The observed variables contain NA values")
  }

  n <- length(solve_variables)

  # nolint start
  data_solve <- regts(matrix(NA, ncol = n), names = solve_variables,
                      period = solve_period)
  # nolint end

  # Function that returns the difference between the observed and calculated
  # value of observed_variables.
  f_solve <- function(x) {
    # Update data_solve with numeric vector x. This is more efficient
    # than creating a new regts object.
    data_solve[] <- x
    mdl$set_data(data_solve)
    mdl$run_eqn(names = equations, period = solve_period,
                solve_order = TRUE)
    retval <- observed_values -
      as.numeric(mdl$get_data(names = observed_variables, period = solve_period))
    return(retval)
  }

  ret <- nleqslv(initial_guess, fn = f_solve, jacobian = jacobian, ...)

  if (!ret$termcd %in% 1:2) {
    stop("Failed to solve the starting value with nleqslv. Message:\n",
         ret$message)
  }

  if (report == "period") {
    cat(glue("Convergence for {solve_period} in {ret$iter} iterations",
             ".\n", .trim = FALSE))
  }

  return(invisible(ret))
}

get_fit_deps <- function(observed_variables, solve_variables,
                         solve_period,  mdl, dep_struc, observed_data, report) {

  observed <- data.frame(var = observed_variables,
                         period = as.character(solve_period))

  derivable <- data.frame(var = solve_variables,
                          period = as.character(solve_period))

  dep_graph <- find_deps_internal(
    final_destinations = observed,
    final_sources = derivable,
    dep_struc = dep_struc,
    observed_data = observed_data,
    mdl = mdl,
    ignore_observed = TRUE,
    stop_if_final_dest_observed = FALSE
  )

  deps <- as_data_frame_deps(dep_graph)

  # Check if the observed variables depend on the solve variables.
  deps_final_src <- filter(deps, .data$is_final_src)
  missing_final_src <- setdiff(solve_variables, deps_final_src$var)
  if (length(missing_final_src) > 0) {
    stop("The observed variable(s) do not depend on the following solve variables:\n",
         paste(missing_final_src, collapse = ", "), ".")
  }

  deps <- dplyr::filter(deps, !.data$is_final_src)

  if (report == "period") {
    cat("\nRunning equations:\n")
    print(deps[, c("var", "period", "is_final_dest", "is_final_src",
                   "calculable")])
  }

  # Check if all equations are calculable.
  if (!all(deps$calculable)) {
    print(deps)
    stop("Equations ",
         paste(filter(deps, !.data$calculable)$var, collapse = ", "),
         " are not calculable.")
  }

  deps
}
