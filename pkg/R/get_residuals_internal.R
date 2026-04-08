#' Function to calculate residuals for a model's endogenous variables
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
#' @return A data frame with columns `name`, `period`, `dif`, `value_lhs`, and `value_rhs`.
#'
#' @importFrom dplyr filter mutate inner_join arrange desc
#' @importFrom rlang .data
#' @import regts
#' @noRd
get_residual_internal <- function(mdl, period, tol, fun) {

  endos <- mdl$get_endo_names()
  lhs_data <- mdl$get_data(names = endos, period = period)
  mdl_temp <- mdl$copy()
  mdl_temp$solve(
    period  = period,
    options = list(mode = "reschk", erropt = "silent")
  )
  rhs_data <- mdl_temp$get_data(names = endos, period = period)

  lhs_long <- regts::as_data_frame(lhs_data, format = "long")
  rhs_long <- regts::as_data_frame(rhs_data, format = "long")

  # Check for NA values
  lhs_long_na <- lhs_long |> dplyr::filter(is.na(.data$value))
  if (nrow(lhs_long_na) > 0) {
    msg <- lhs_long_na |>
      dplyr::group_by(.data$name) |>
      dplyr::summarize(periods = paste(as.character(.data$period), collapse = ", "),
                       .groups = "drop") |>
      dplyr::mutate(line = paste0("  ", .data$name, ": ", .data$periods)) |>
      dplyr::pull(.data$line) |>
      paste(collapse = "\n")
    message("Voor de volgende lhs variabelen ontbreken waarnemingen in de data voor deze perioden:\n", msg)
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
    message("De volgende rhs variabelen konden niet worden berekend:\n", msg)
  }

  rhs_long_ok <- rhs_long |>
    dplyr::filter(!is.na(.data$value))

  # Alleen namen/perioden waarvoor lhs berekenbaar is (rhs_long_ok).
  # Gebruik suffixes _lhs en _rhs voor de waarden.
  residuals <- inner_join(lhs_long, rhs_long_ok,
                                 by = c("name", "period"),
                                 suffix = c("_lhs", "_rhs")) |>
    mutate(dif = fun(.data$value_lhs, .data$value_rhs), .after = "period") |>
    dplyr::filter(abs(.data$dif) >= tol | is.na(.data$dif)) |>
    # NA-waarden komen alleen voor als de lhs NA is. Komen bovenaan.
    arrange(desc(is.na(.data$dif)), desc(abs(.data$dif)))

  return(residuals)
}