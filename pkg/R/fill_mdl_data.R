#' Solves a model
#'
#'This procedure attempts to calculate missing data for endogenous
#'variables of a model by solving the identity equations in solution order.

#' When there are no data for an endogenous variable in a period
#' and that variable is determined by an identity, a value could be
#' calculated if all right-hand side variables used in the equation
#' were available. If running the equation results in a valid
#' number, the missing value will be replaced. The procedure will
#' never overwrite existing valid data.

#' The procedure can be used to fill in data before and beyond the
#' model data period (as set by @proc{setmdp}) for as many
#' variables as possible.
#'
#' @param mdl an \code{\link{IsisMdlS4}} object
#' @param  period a \code{\link[regts]{regperiod_range}} object
#' @return an \code{\link{IsisMdlS4}} object
#' @export
#' @export
fill_mdl_data <- function(mdl, period = mdl@data_period) {

    period <- as.regperiod_range(period)

    prepare_mws(mdl, period, solve = FALSE)

    nper   <- as.integer(length_range(period))
    .Call("filmdt_c", model_index = mdl@control$index,
          jtb = 1L, jte = nper)

    return(update_data_ca(mdl, period))
}
