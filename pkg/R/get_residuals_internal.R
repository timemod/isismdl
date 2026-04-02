#' Function to calculate residuals for a model's endogenous variables
#' @import regts
#' @import dplyr
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

  na_rows <- rhs_long[is.na(rhs_long$value), ]
  if (nrow(na_rows) > 0) {
    message(sprintf("The following %d calculation(s) resulted in missing values
                    (NA) and have been excluded from the comparison:", nrow(na_rows)))
    print(na_rows)
  }

  intersect_vars <- intersect(colnames(lhs_data), colnames(rhs_data))
  message("The following variables were found in both datasets
          and will be included in the comparison:")
  print(intersect_vars)

  dif <- fun(lhs_data, rhs_data)

  residuals <- dif |>
    regts::as_data_frame(format = "long", value_col = "dif") |>
    dplyr::filter(abs(dif) >= tol | is.na(abs(dif))) |>
    dplyr::arrange(dplyr::desc(abs(dif))) |>
    dplyr::left_join(lhs_long, by = c("name", "period")) |>
    dplyr::rename(value_derived = "value") |>
    dplyr::left_join(rhs_long, by = c("name", "period")) |>
    dplyr::rename(value_reschk = "value")

  return(residuals)
}
