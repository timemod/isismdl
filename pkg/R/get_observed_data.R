#' @importFrom regts as_data_frame
#' @importFrom dplyr filter

get_observed_data = function(data_init) {
  observed_data <- regts::as_data_frame(data_init, format = "long",
                                        name_col = "var", period_col = "period") |>
    dplyr::filter(!is.na(.data$value))

  return(observed_data)
}
