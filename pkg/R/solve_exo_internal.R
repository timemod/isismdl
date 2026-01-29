#' Solve exogenous variables numerically for a given period
#' @import regts
#' @import nleqslv
#' @importFrom rlang .data
#' @noRd

solve_exo_internal <- function(
  mdl,
  solve_period,
  exo_vars,
  target_vars,
  report,
  jacobian = TRUE,
  ...
) {
  if (length(exo_vars) != length(target_vars)) {
    stop("Length of 'exo_vars' must be equal to length of 'target_vars'.")
  }
  # Check target vars = endo & exo_vars = exo
  periods <- seq(
    regts::start_period(solve_period),
    regts::end_period(solve_period)
  )

  for (per in as.list(periods)) {
    if (report == "period") {
      cat("\n===============================================\n")
      cat(
        "Solving exogenous variables for period ",
        as.character(per),
        "\n",
        sep = ""
      )
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
      cat("===============================================\n\n")
    }

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

    n <- length(exo_vars)

    current_exo <- mdl$get_data(names = exo_vars, period = per) |>
      as.numeric()

    if (all(is.na(current_exo))) {
      current_exo[] <- 0
    } else if (any(is.na(current_exo))) {
      current_exo[is.na(current_exo)] <- 0
    }

    f_exo <- function(x) {
      data_exo <- regts::regts(
        matrix(x, ncol = n),
        names = exo_vars,
        period = per
      )
      mdl$set_data(data_exo)

      mdl$solve(period = per, options = list(report = "no"))

      # Compute residuals: observed/target - model value
      model_targets <- mdl$get_data(names = target_vars, period = per) |>
        as.numeric()

      retval <- target_values - model_targets
      cat("x:", x, "residuals:", retval, "\n")
      return(retval)
    }

    # If we don’t want a period-level report, no need to store jacobian
    current_jacobian <- if (report == "period") jacobian else FALSE

    ret <- nleqslv::nleqslv(
      x = current_exo,
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

    # On success, set the solved exogenous values in the model
    solved_exo <- regts::regts(
      matrix(ret$x, ncol = n),
      names = exo_vars,
      period = per
    )
    mdl$set_data(solved_exo)
  }

  invisible(NULL)
}
