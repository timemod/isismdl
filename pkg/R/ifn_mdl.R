#' Returns an IFN model

#' This function returns an unitialized IFN model.
#' @return a \code{\link{IsisMdl}} object
#' @examples
#' mdl <- ifn_mdl()
#' @export
ifn_mdl <- function() {
    mdl_file <- tempfile(fileext = ".mdl")
    copy_example_mdl("ifn", mdl_file)
    retval <- isis_mdl(mdl_file)
    unlink(mdl_file)
    return(retval)
}
