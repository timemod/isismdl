#' @export
setGeneric("saveRDS")

#' Writes a model file to a RDS file
#'
#' @useDynLib isismdl write_mdl_c
#' @importFrom methods getSlots
#' @importFrom methods slot
#' @export
setMethod("saveRDS", "IsisMdlS4",
    function(object, file) {

    # TODO: use tempfile
    mif_file <- "saveRDS.mif"
    .Call("write_mdl_c", mif_file, object@model_index)
    size <- file.info(mif_file)$size
    mif_data <- readBin(mif_file, what = "raw", n = size)

    slot_names <- names(getSlots("IsisMdlS4"))
    slot_names <- setdiff(slot_names, c("model_index"))
    serialized_mdl <-lapply(slot_names, FUN = function(x) {slot(object, x)})
    names(serialized_mdl) <- slot_names
    serialized_mdl$mif_data <- mif_data
    saveRDS(serialized_mdl, file)
    unlink(mif_file)
    return (invisible(NULL))
}
)
