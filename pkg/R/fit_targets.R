#' Retrieve or update the fit targets
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param names a character vector with variable names
#' @param value a \code{\link[ts]{ts}} or \code{\link[regts]{regts}}
#'  or an object that can be coerced to a \code{regperiod_range}
#' @export
fit_targets <- function(mdl) {
    return(mdl@fit)
}

#' @rdname fit_targets
#' @importFrom regts get_regperiod_range
#' @export
`fit_targets<-` <- function(mdl, names = colnames(value), value) {
    return(update_data(mdl, "fit",  value, names, missing(names)))
}
