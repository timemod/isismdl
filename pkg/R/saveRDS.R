#' Save an object to an RDS file.
#'
#' The special version for \code{\link{IsisMdl}} objects takes care of the
#' equation code stored in Fortran memory.
#' @name saveRDS
#' @rdname saveRDS-methods
#' @param object an \code{\link{IsisMdl}} object
#' @param file a \code{\link[base]{connection}} or the name of the file where
#' @exportMethod saveRDS
setGeneric("saveRDS")

#' @rdname saveRDS-methods
#' @aliases saveRDS,IsisMdlS4-method
#' @importFrom methods getSlots
#' @importFrom methods slot
#' @export
setMethod("saveRDS", "IsisMdlS4",
    function(object, file) {

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
    saveRDS(serialized_mdl, file)
    unlink(mif_file)
    return (invisible(NULL))
}
)
