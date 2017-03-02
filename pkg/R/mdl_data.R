#' Retrieve timeseries from the model data, constant adjusments, fix values
#' or fit targets
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param names a character vector with variable names
#' @param pattern a regular expression
#' @param period an \code{\link[regts]{regperiod_range}} object
#' @name mdl_data
NULL

#' @describeIn mdl_data Model data
#' @export
mdl_data <- function(mdl, names, pattern, period = mdl@data_period) {
    period <- as.regperiod_range(period)
    if (missing(pattern) && missing(names)) {
        names <- mdl@names
    } else if (missing(names)) {
        names <- get_var_names(mdl, pattern)
    } else if (!missing(pattern)) {
        names <- union(names, get_var_names(mdl, pattern))
    }
    return(mdl@data[period, names, drop = FALSE])
}

#' @describeIn mdl_data Constant adjustments
#' @export
ca <- function(mdl, names, pattern, period = mdl@data_period) {
    period <- as.regperiod_range(period)
    if (missing(pattern) && missing(names)) {
        names <- mdl@ca_names
    } else if (missing(names)) {
        names <- grep(pattern, mdl@ca_names)
    } else if (!missing(pattern)) {
        names <- union(names, grep(pattern, mdl@ca_names))
    }
    return(mdl@ca[period, names, drop = FALSE])
}

#' @describeIn mdl_data Fix values
#' @export
fix_values <- function(mdl) {
     return(mdl@fix)
}

#' @describeIn mdl_data Fit targets
#' @export
fit_targets <- function(mdl) {
     return(mdl@fit)
}
