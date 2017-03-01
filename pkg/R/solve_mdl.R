#' Solves a model
#'
#' @param mdl an \code{\link{IsisMdlS4}} object
#' @param  period a \code{\link[regts]{regperiod_range}} object
#' @param options a list with solve options
#' @param fit_options a list with options for the fit procedure
#' @return an \code{\link{IsisMdlS4}} object
#' @export
solve_mdl <- function(mdl, period = mdl@period, options = list(),
                      fit_options = list()) {

    period <- as.regperiod_range(period)

    prepare_mws(mdl, period)

    cat("time for preparing  the mws\n")
    print(t)

    nper   <- as.integer(length_range(period))
    .Call("solve_c", model_index = mdl@control$index,
           jtb = 1L, jte = nper, options, fit_options)

    return(update_data_ca(mdl, period))
}
