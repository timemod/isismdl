#' Transfers data from a timeseries object to the model data, constant adjusments,
#' fix values or fit targets.
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param data a \code{\link[ts]{ts}} or \code{\link[regts]{regts}}
#'  or an object that can be coerced to a \code{regperiod_range}
#' @param names a character vector with variable names. Defaults to the
#' column names of \code{data}. If \code{data} does not have column names,
#' then argument \code{names} is mandatory
#' @name set_data
NULL


#' @describeIn set_data Set model data
#' @export
set_data <- function(mdl, data, names = colnames(data)) {
    return(set_data_(mdl, "data",  data, names, missing(names)))
}

#' @describeIn set_data Set constant adjustments
#' @export
set_ca <- function(mdl, data, names = colnames(data)) {
    return(set_data_(mdl, "ca",  data, names, missing(names)))
}

#' @describeIn set_data Set fix values
#' @export
set_fix <- function(mdl, data, names = colnames(data)) {
    return(set_data_(mdl, "fix",  data, names, missing(names)))
}

#' @describeIn set_data Set fit values
#' @export
set_fit <- function(mdl, data, names = colnames(data)) {
    return(set_data_(mdl, "fit",  data, names, missing(names)))
}
