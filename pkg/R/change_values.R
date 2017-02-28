#' Change the model data.
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param fun an function
#' @param names a character vector with variable names
#' @param pattern a regular expression
#' @param period an \code{\link[regts]{regperiod_range}} object
#' @param value a \code{\link[ts]{ts}} or \code{\link[regts]{regts}}
#'  or an object that can be coerced to a \code{regperiod_range}
#' @importFrom regts as.regperiod_range
#' @export
change_values <- function(mdl, fun, names = NULL, pattern = NULL,
                           period = mdl@data_period) {

    period <- as.regperiod_range(period)
    nper <- length_range(period)

    if (!missing(names)) {
        names <- intersect(mdl@names, names)
    }
    if (is.null(pattern) && is.null(names)) {
        names <- mdl@names
    } else if (is.null(names)) {
        stop("pattern not yet supported")
    } else if (!is.null(pattern)) {
        stop("pattern not yet supported")
      #names <- union(names, self$get_var_names(pattern))
   }

    period <- regrange_intersect(mdl@data_period, period)
    mdl@data[period, names] <- fun(mdl@data[period, names])
    return(invisible(mdl))
}
