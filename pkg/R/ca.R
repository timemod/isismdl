#' Retrieve or update the constant adjusmtments
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param names a character vector with variable names
#' @param pattern a regular expression
#' @param period an \code{\link[regts]{regperiod_range}} object
#' @param value a \code{\link[ts]{ts}} or \code{\link[regts]{regts}}
#'  or an object that can be coerced to a \code{regperiod_range}
#' @export
ca <- function(mdl, names, pattern, period = mdl@data_period) {
    period <- as.regperiod_range(period)
    if (missing(pattern) && missing(names)) {
        names <- mdl@ca_names
    } else if (missing(names)) {
        #names <- self$get_var_names(pattern)
        stop("Argument pattern not yet supported")
    } else if (!missing(pattern)) {
        stop("Argument pattern not yet supported")
        #names <- union(names, self$get_var_names(pattern))
    }
    return(mdl@ca[period, names])
}

#' @rdname ca
#' @importFrom regts get_regperiod_range
#' @export
`ca<-` <- function(mdl, names = colnames(value), value) {
    return(update_data(mdl, "ca",  value, names, missing(names)))
}
