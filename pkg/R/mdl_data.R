#' Retrieve or set the model data.
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param names a character vector with variable names
#' @param pattern a regular expression
#' @param period an \code{\link[regts]{regperiod_range}} object
#' @param value a \code{\link[ts]{ts}} or \code{\link[regts]{regts}}
#'  or an object that can be coerced to \code{regperiod_range}
#' @export
setGeneric("mdl_data",   function(mdl, names, pattern, period = mdl@period, ...)
                                       standardGeneric("mdl_data"))

#' @rdname mdl_data
#' @export
setGeneric("mdl_data<-", function(mdl, value) standardGeneric("mdl_data<-"))


#' @rdname mdl_data
#' @export
setMethod("mdl_data", "IsisMdlS4",
          function(mdl, names, pattern, period = mdl@data_period) {
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
              return(mdl@data[period, names])
          }
)

#' @rdname mdl_data
#' @importFrom regts get_regperiod_range
#' @importFrom methods loadMethod
#' @export
setMethod("mdl_data<-", "IsisMdlS4",
          function(mdl, value) {
              data <- create_data(mdl@names, mdl@data_period)
              if (!is.ts(value)) {
                  stop("value is not a timeseries object")
              }
              value <- convert_data(value, names = mdl@names,
                                    period = mdl@data_period)
              mdl@data[get_regperiod_range(value), colnames(value)] <- value
              return(invisible(mdl))
          }
)



