#' Function to calculate residuals for the equations of the model.
#'
#' This function calculates the difference between the values of endogenous
#' variables in the model data and the values calculated from the model
#' equations (residual check mode).
#'
#' @param mdl An IsisMdl object.
#' @param period The period range for which to calculate residuals.
#' @param tol The tolerance for including residuals in the output.
#' @param fun The function used to calculate the difference (e.g., `regts::cvgdif`).
#'
#' @return A data frame with columns `name`, `period`, `residuals`, `value_lhs`, and `value_rhs`.
#'
#' @importFrom dplyr filter mutate inner_join arrange desc group_by summarize pull select any_of
#' @importFrom rlang .data
#' @import regts
#' @noRd
get_residual_internal <- function(mdl, names, period, tol, fun) {

  if (is.null(names)) {
    names <- mdl$get_endo_names(status = "active")
  }
  lhs_data <- mdl$get_data(names = names, period = period)
  mdl_temp <- mdl$copy()
  mdl_temp$solve(
    period  = period,
    options = list(mode = "reschk", erropt = "silent", report = "none")
  )
  rhs_data <- mdl_temp$get_data(names = names, period = period)

  lhs_long <- regts::as_data_frame(lhs_data, format = "long")
  rhs_long <- regts::as_data_frame(rhs_data, format = "long")

  # Check for NA values
  all_na_msg <- ""

  lhs_long_na <- lhs_long |> dplyr::filter(is.na(.data$value))
  if (nrow(lhs_long_na) > 0) {
    msg <- lhs_long_na |>
      dplyr::group_by(.data$name) |>
      dplyr::summarize(periods = paste(as.character(.data$period), collapse = ", "),
                       .groups = "drop") |>
      dplyr::mutate(line = paste0("  ", .data$name, ": ", .data$periods)) |>
      dplyr::pull(.data$line) |>
      paste(collapse = "\n")
    all_na_msg <- paste(
      all_na_msg,
      paste0("For the following lhs variables, observations
             are missing in the data for these periods:\n", msg),
      sep = if (nchar(all_na_msg) > 0) "\n\n" else ""
    )
  }

  rhs_long_na <- rhs_long |> dplyr::filter(is.na(.data$value))
  if (nrow(rhs_long_na) > 0) {
    msg <- rhs_long_na |>
      dplyr::group_by(.data$name) |>
      dplyr::summarize(periods = paste(as.character(.data$period), collapse = ", "),
                       .groups = "drop") |>
      dplyr::mutate(line = paste0("  ", .data$name, ": ", .data$periods)) |>
      dplyr::pull(.data$line) |>
      paste(collapse = "\n")
    all_na_msg <- paste(
      all_na_msg,
      paste0("The following rhs variables could not be calculated:\n", msg),
      sep = if (nchar(all_na_msg) > 0) "\n\n" else ""
    )
  }

  if (nchar(all_na_msg) > 0) {
    message(all_na_msg)
  }

  rhs_long_ok <- rhs_long |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::select(-dplyr::any_of("label"))

  lhs_long_ok <- lhs_long |>
    dplyr::filter(!is.na(.data$value))

  # Only names and periods that are not calculable (rhs_long_ok).
  # Use suffixes _lhs en _rhs for the values.
  residuals <- dplyr::inner_join(lhs_long_ok, rhs_long_ok,
                                 by = c("name", "period"),
                                 suffix = c("_lhs", "_rhs")) |>
    dplyr::mutate(residuals = fun(.data$value_lhs, .data$value_rhs), .after = "period") |>
    dplyr::filter(abs(.data$residuals) >= tol | is.na(.data$residuals)) |>
    dplyr::arrange(dplyr::desc(is.na(.data$residuals)), dplyr::desc(abs(.data$residuals)))

  return(residuals)
}
