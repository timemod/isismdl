#' The S4 variant of fill_mdl_data

#' @export
fill_mdl_data <- function(mdl, data, ca = NULL, period) {
    period <- as.regperiod_range(period)
    prepare_mws(mdl@model_index, period, data, ca, NULL, NULL)

    js <- get_period_indices(period, period)
    .Call("filmdt_c", model_index = mdl@model_index,
          jtb = js$startp, jte = js$end)

    return(get_solution(mdl@model_index, period))
}
