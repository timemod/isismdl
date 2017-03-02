#' Set the solve options
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param ... the solve options (TODO: document the possible options)
#' @export
set_solve_options <- function(mdl, ...) {

    x <- list(...)
    print(x)

    names <- intersect(names(x), names(mdl@solve_opts))
    mdl@solve_opts[names] <- x[names]

    return (invisible(mdl))
}
