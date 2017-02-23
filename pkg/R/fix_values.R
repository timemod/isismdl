#' Retrieve or set the fix values
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param names a character vector with variable names
#' @param pattern a regular expression
#' @param period an \code{\link[regts]{regperiod_range}} object
#' @param value a \code{\link[ts]{ts}} or \code{\link[regts]{regts}}
#'  or an object that can be coerced to \code{regperiod_range}
#' @export
#' @export
setGeneric("fix_values",
          function(mdl, names, pattern, period = mdl@period, ...)
                               standardGeneric("fix_values"))

#' @rdname fix_values
#' @export
setGeneric("fix_values<-", function(mdl, value) standardGeneric("fix_values<-"))

#' @rdname fix_values
#' @export
setMethod("fix_values", "IsisMdlS4",
          function(mdl) {
              return(mdl@fix)
          }
)

#' @rdname fix_values
#' @importFrom regts get_regperiod_range
#' @importFrom methods setMethod
#' @importFrom methods loadMethod
#' @export
setMethod("fix_values<-", "IsisMdlS4",
          function(mdl, value) {
              mdl@fix <- convert_data(value, names = mdl@ca_names,
                                      period = mdl@period)
              return(invisible(mdl))
          }
)

