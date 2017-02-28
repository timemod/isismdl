#' Returns the equation names
#'
#' Returns the equation names
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param pattern a regular expression
#' @param type the type (all or inactive)
#' @useDynLib isismdl get_eq_names_c
#' @export
eq_names <- function(mdl, pattern = ".*", type =  c("all", "inactive")) {
    type <- match.arg(type)
    names <- .Call("get_eq_names_c", type, mdl@control$index)
    if (!missing(pattern)) {
        sel <- grep(pattern, names)
        names <- names[sel]
    }
    return(names)
}
