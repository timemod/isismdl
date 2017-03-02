#' Sets the values of specific model variables.
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param value a numeric vector of length 1 or with the same length
#' as the length of the range of \code{period}.
#' @param names a character vector with variable names
#' @param pattern a regular expression
#' @param period an \code{\link[regts]{regperiod_range}} object
#' @name set_values
NULL

#' @describeIn set_values Sets the values of model variables
#' @export
set_values <- function(mdl, value, names, pattern, period = mdl@data_period) {
    return(set_values_(mdl, "data",  value, names, pattern, period))
}

#' @describeIn set_values Sets the values of model variables
#' @export
set_ca_values <- function(mdl, value, names, pattern, period = mdl@period) {
    return(set_values_(mdl, "ca",  value, names, pattern, period))
}

#' @describeIn set_values Sets the values of the fix values
#' @export
set_fit_values <- function(mdl, value, names, pattern, period = mdl@period) {
    return(set_values_(mdl, "fit",  value, names, pattern, period))
}

#' @describeIn set_values Sets the values of the fit targets
#' @export
set_fix_values <- function(mdl, value, names, pattern, period = mdl@period) {
    return(set_values_(mdl, "fix",  value, names, pattern, period))
}
