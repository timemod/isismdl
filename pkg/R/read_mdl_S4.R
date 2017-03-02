#' Reads a model from an RDS file (S4 version)
#'
#' This function reads a model from an RDS file that was generated
#' by \code{\link{IsisMdl}} method \code{saveRDS},
#' @param file the name of the RDS file
#' @return a \code{\link{IsisMdl}} object
#' @export
read_mdl_S4 <- function(file) {

    serialized_mdl <- readRDS(file)

    # TODO: use tempfile for mif name and delete the mif file at the
    # end of this function
    mif_file <- "read_mdl.mif"
    writeBin(serialized_mdl$mif_data, con = mif_file)
    model_index <- .Call(read_mdl_c, mif_file)
    unlink(mif_file)

    slots <- serialized_mdl
    slots$mif_data <- NULL
    slots$control <- ModelControl$new(model_index)
    return(do.call(IsisMdlS4, slots))
}
