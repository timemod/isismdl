#' Sets or updates  the rms values
#'
#' @param mdl an \code{\link{IsisMdl}} object
#' @param values a named numeric vector with rms values
#' @export
set_rms <- function(mdl, values) {

    # as.numeric does not preserve names, therefore save them
    names <- names(values)
    names <- intersect(names, mdl@ca_names)

    values <- as.numeric(values[names])

    if (is.null(mdl@rms)) {
        mdl@rms <- values
        names(mdl@rms) <- names
    } else {
        mdl@rms[names] <- values
    }

    return(invisible(mdl))
}
