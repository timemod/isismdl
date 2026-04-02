#' Solve exogenous variables numerically for a given period
#' @import regts
#' @import nleqslv
#' @importFrom rlang .data
#' @importFrom glue glue
#' @noRd
solve_exo_internal <- function(
  mdl,
  solve_period,
  exo_vars,
  target_vars,
  report,
  jacobian,
  ...
) {
  if (length(exo_vars) != length(target_vars)) {
    stop("Length of 'exo_vars' must be equal to length of 'target_vars'.")
  }
  if (length(mdl$get_endo_names(type = "feedback")) > 0) {
    stop("solve_exo does not support models with feedback variables.")
  }
  # Check whether target vars are endogenous & exo_vars are exogenous
  all_endo <- mdl$get_endo_names(status = "active")
  invalid_targets <- setdiff(target_vars, all_endo)
  if (length(invalid_targets) > 0) {
    stop(
      "The following target variables are not active endogenous: ",
      paste(invalid_targets, collapse = ", ")
    )
  }

  all_exo <- mdl$get_exo_names()
  invalid_exo <- setdiff(exo_vars, all_exo)
  if (length(invalid_exo) > 0) {
    stop(
      "The following variables in 'exo_vars' are not exogenous: ",
      paste(invalid_exo, collapse = ", ")
    )
  }

  if (report != "none") {
    cat("\n===============================================\n")
    cat(
      "Solving exogenous variables for period ",
      as.character(solve_period),
      "\n",
      sep = ""
    )
    cat("===============================================\n\n")
    cat(
      "Exogenous variables: ",
      paste(exo_vars, collapse = ", "),
      "\n",
      sep = ""
    )
    cat(
      "Target variables:    ",
      paste(target_vars, collapse = ", "),
      "\n",
      sep = ""
    )
  }

  periods <- seq(
    regts::start_period(solve_period),
    regts::end_period(solve_period)
  )

  for (per in as.list(periods)) {

    target_values <- mdl$get_data(names = target_vars, period = per) |>
      as.numeric()

    if (any(is.na(target_values))) {
      missing_targets <- target_vars[is.na(target_values)]
      stop(
        "solve_exo: target variables contain NA values in period ",
        as.character(per),
        ": ",
        paste(missing_targets, collapse = ", ")
      )
    }

    # Use current values in model data as starting values for the solver.
    # If the current value is NA, use 0.
    data_exo <-  mdl$get_data(names = exo_vars, period = per)
    if (anyNA(data_exo)) {
      data_exo[is.na(data_exo)] <- 0
    }

    if (report == "period") cat("\n")

    f_exo <- function(x) {

      # Update data_exo with numeric vector x. This is more efficient
      # than creating a new regts object.
      data_exo[] <- x

      mdl$set_data(data_exo)

      mdl$run_eqn(period = per)

      # Compute residuals: observed/target - model value
      model_targets <- mdl$get_data(names = target_vars, period = per) |>
        as.numeric()

      retval <- target_values - model_targets
      return(retval)
    }

    # If we don’t want a period-level report, no need to store jacobian
    current_jacobian <- if (report == "period") jacobian else FALSE

    ret <- nleqslv::nleqslv(
      x = as.numeric(data_exo),
      fn = f_exo,
      jacobian = current_jacobian,
      ...
    )

    if (current_jacobian && report == "period") {
      cat("Final jacobian for exogenous variables:\n")
      jac <- ret$jac
      rownames(jac) <- target_vars
      colnames(jac) <- exo_vars
      print(jac)
      cat("\n")
    }

    # Check convergence
    if (!ret$termcd %in% 1:2) {
      stop(
        "solve_exo_internal: Failed to solve exogenous variables for period ",
        as.character(per),
        " with nleqslv.\n",
        "Message from nleqslv: ",
        ret$message
      )
    }

    if (report == "period") {
      cat(glue("Convergence for {per} in {ret$iter} iterations",
               ".\n", .trim = FALSE))
    }
  }

  invisible(mdl)
}
