#' Reads a model from an RDS file
#'
#' This function reads a model from an RDS file that was generated
#' by \code{\link{IsisMdl}} method \code{saveRDS},
#' @param file the name of the RDS file
#' @return a \code{\link{IsisMdl}} object
#' @export
read_mdl <- function(file) {
    serialized_mdl <- readRDS(file)
    # TODO: use tempfile for mif name and delete the mif file at the
    # end of this function
    mif_file <- "read_mdl.mif"
    writeBin(serialized_mdl$mif_data, con = mif_file)
    ret <- IsisMdl$new(mif_file)
    ret$set_mws(serialized_mdl$mws)
    unlink(mif_file)
    return(ret)
}
