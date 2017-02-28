#' Returns the names of the model variables
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param pattern a regular expression
#' @param type the variable type (see Isis Reference Manual)
#' @useDynLib isismdl get_var_names_c
#' @export
get_var_names <- function(mdl, pattern = ".*",
                     type = c("all", "allfrml", "all_endolead")) {
    type <- match.arg(type)
    names <- .Call("get_var_names_c", type, mdl@model_index)
    if (!missing(pattern)) {
        sel <- grep(pattern, names)
        names <- names[sel]
    }
    return(names)
}
