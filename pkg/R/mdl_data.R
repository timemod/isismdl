#' Retrieve or update the model data.
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param names a character vector with variable names
#' @param pattern a regular expression
#' @param period an \code{\link[regts]{regperiod_range}} object
#' @param value a \code{\link[ts]{ts}} or \code{\link[regts]{regts}}
#'  or an object that can be coerced to a \code{regperiod_range}
#' @rdname mdl_data
#' @export
mdl_data <- function(mdl, names, pattern, period = mdl@data_period) {
    period <- as.regperiod_range(period)
    if (missing(pattern) && missing(names)) {
        names <- mdl@names
    } else if (missing(names)) {
        #names <- self$get_var_names(pattern)
        stop("Argument pattern not yet supported")
    } else if (!missing(pattern)) {
        stop("Argument pattern not yet supported")
        #names <- union(names, self$get_var_names(pattern))
    }
    return(mdl@data[period, names, drop = FALSE])
}

#' @rdname mdl_data
#' @importFrom regts get_regperiod_range
#' @export
`mdl_data<-` <- function(mdl, names = colnames(value), value) {
    return(update_data(mdl, "data",  value, names, missing(names)))
}



