#' The S4 variant of solve_mdl

#' @export
solve_mdl <- function(mdl, data, ca = NULL, fix = NULL, fit = NULL,
                      period, options = list(), fit_options = list()) {

    period <- as.regperiod_range(period)

    prepare_mws(mdl@model_index, period, data, ca, fix, fit)

    js <- get_period_indices(period, period)
    .Call("solve_c", model_index = mdl@model_index,
          jtb = js$startp, jte = js$endp, options,
          fit_options)

    return(get_solution(mdl@model_index, period))

}
