#' Changes the model data or constant adjustments by applying a function.
#'
#' @param mdl an \code{\link{IsisMdlS4}} object
#' @param fun an function
#' @param names a character vector with variable names
#' @param pattern a regular expression
#' @param period an \code{\link[regts]{regperiod_range}} object
#' @param ... arguments passed to \code{fun}
#' @name change_data
NULL

#' @describeIn change_data Change the model variables
#' @export
change_data <- function(mdl, fun, names = NULL, pattern = NULL,
                          period = mdl@data_period) {
    return(change_values_(mdl, "data", fun, names, pattern, period))
}

#' @describeIn change_data Change the constant adjusments
#' @export
change_ca <- function(mdl, fun, names = NULL, pattern = NULL,
                        period = mdl@period) {
    return(change_values_(mdl, "ca", fun, names, pattern, period))
}
