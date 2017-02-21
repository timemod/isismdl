#' Returns an ISLM model
#'
#' This function returns an unitialized ISLM model.
#' @return a \code{\link{IsisMdl}} object
#' @examples
#' mdl <- islm_mdl()
#' @export
islm_mdl <- function() {
    mdl_file <- tempfile(fileext = ".mdl")
    copy_example_mdl("islm", mdl_file)
    retval <- isis_mdl(mdl_file)
    unlink(mdl_file)
    return(retval)
}
