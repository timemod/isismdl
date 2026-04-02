#' Function to calculate residuals for a model's endogenous variables
# RVH: je moet nooit een heel pakket importeren, alleen een individuele functie.
# Ik krijg anders waarschuwingen als
# Warning: replacing previous import ‘dplyr::as_data_frame’ by ‘regts::as_data_frame’ when loading ‘isismdl’
#** checking absolute paths in shared objects and dynamic libraries
#** testing if installed package can be loaded from final location
# @import regts
# @import dplyr
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

  # RVH: foutmelding is niet duidelijk. Je wilt de namen van de lhs-variabelen
  # en de perioden.
  #na_rows <- rhs_long[is.na(rhs_long$value), ]
  #if (nrow(na_rows) > 0) {
  #  message(sprintf("The following %d calculation(s) resulted in missing values
  #                  (NA) and have been excluded from the comparison:", nrow(na_rows)))
  #  print(na_rows)
  #}
  rhs_long_na <- filter(rhs_long, is.na(.data$value))
  #  TODO: geef een message waarin een overzicht met alle namen en perioden
  # in lhs_long_na. De boodschap moet iets zijn zoalsals :
  # De volgende lhs variabelen konden niet worden berekend in perioden.



  # RVH: deze boodschap vind ik niet nodig, resultaat komt is verschil
  # intersect_vars <- intersect(colnames(lhs_data), colnames(rhs_data))
  # message("The following variables were found in both datasets
  #         and will be included in the comparison:")
  # print(intersect_vars)
  #

  # dif <- fun(lhs_data, rhs_data)
  #
  # residuals <- dif |>
  #   regts::as_data_frame(format = "long", value_col = "dif") |>
  #   dplyr::filter(abs(dif) >= tol | is.na(abs(dif))) |>
  #   dplyr::arrange(dplyr::desc(abs(dif))) |>
  #   dplyr::left_join(lhs_long, by = c("name", "period")) |>
  #   dplyr::rename(value_derived = "value") |>
  #   dplyr::left_join(rhs_long, by = c("name", "period")) |>
  #   dplyr::rename(value_reschk = "value")


  # Met bovenstaande code komen rijen waarvoor de rhs NA is nog steeds voor, want
  # dif bevat alle verschillen.
  # En de namen van de kolommen moeten zijn: value_lhs en value_rhs, niet value_derived
  # en value_reschk. Bovendien kan de code veel simpeler:
  rhs_long_ok <-  filter(rhs_long, !is.na(.data$value))

  # Alleen namen/perioden waarvoor lhs berekenbaar is! Daarom right_join.
  residuals <- dplyr::right_join(lhs_long, rhs_long_ok, by = c("name", "period"),
                       suffix = c("_lhs", "_rhs")) |>
    mutate(dif = fun(.data$value_lhs, .data$value_rhs), .after = "period") |>
    filter(abs(dif) >= tol | is.na(abs(dif))) |>
    # RVH: vraagje: wat gebeurt er met NA-waarden? Komen deze bovenaan of onderaan?
    # Bovenaan lijkt me OK. NA-waarden komen alleen
    # voor als de lhs NA is. Ik weet het niet, even testen,
    dplyr::arrange(dplyr::desc(abs(dif)))

  return(residuals)
}
