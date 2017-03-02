#' Save an object to an RDS file.
#'
#' The special version for \code{\link{IsisMdlS4}} objects takes care of the
#' equation code stored in Fortran memory.
#' @name saveRDS
#' @param object R object to serialize (for example an \code{\link{IsisMdlS4}}
#' object)
#' @param file a connection or the name of the file where the R object is saved
#' to or read from.
#' @param ascii	 a logical. If \code{TRUE} or \code{NA}, an ASCII representation
#' is written; otherwise (default), a binary one is used. See the comments
#' in the help for save.
#' @param version the workspace format version to use. \code{NULL} specifies
#' the current default version (2). Versions prior to 2 are not supported, so
#' this will only be relevant when there are later versions.
#' @param compress a logical specifying whether saving to a named file is
#' to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate
#'  the type of compression to be used. Ignored if file is a connection.
#' @param refhook a hook function for handling reference objects.
#' @rdname saveRDS-methods
#' @exportMethod saveRDS
setGeneric("saveRDS")

#' @rdname saveRDS-methods
#' @aliases saveRDS,IsisMdlS4-method
#' @importFrom methods getSlots
#' @importFrom methods slot
#' @export
setMethod("saveRDS", "IsisMdlS4",
    function(object, file, ascii, version, compress, refhook) {

    # TODO: use tempfile
    mif_file <- "saveRDS.mif"
    .Call("write_mdl_c", mif_file, object@control$index)
    size <- file.info(mif_file)$size
    mif_data <- readBin(mif_file, what = "raw", n = size)

    slot_names <- names(getSlots("IsisMdlS4"))
    slot_names <- setdiff(slot_names, "control")
    serialized_mdl <-lapply(slot_names, FUN = function(x) {slot(object, x)})
    names(serialized_mdl) <- slot_names
    serialized_mdl$mif_data <- mif_data
    saveRDS(serialized_mdl, file, ascii, version, compress, refhook)
    unlink(mif_file)
    return (invisible(NULL))
}
)
