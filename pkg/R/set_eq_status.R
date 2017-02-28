#'  Sets the the equation status (active or inactive) of one or more equations.
#'
#' @param pattern a regular expression
#' @param names a character vector with the equation names
#' @param status equation status (\code{"active"} or \code{"inactive"})
#' @export
set_eq_status <- function(mdl, pattern, names,
                          status = c("active", "inactive")) {

    status <- match.arg(status)

    eq_nms <- eq_names(mdl)

    if (!missing(names)) {
        names <- intersect(names, eq_nms)
    }

    if (missing(pattern) && missing(names)) {
        names <- eq_nms
    } else if (!missing(pattern)) {
        eq_vars <- eq_names(mdl, pattern)
        if (!missing(names)) {
            names <- union(eq_vars, names)
        } else {
            names <- eq_vars
        }
    }

    if (status == "active") {
        mdl@inactive_eqs <- setdiff(mdl@inactive_eqs, names)
    } else {
        mdl@inactive_eqs <- union(mdl@inactive_eqs, names)
    }

    return(invisible(mdl))
}
