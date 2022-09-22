#' @useDynLib isismdl remove_all_mwss_c
.onUnload <- function(libpath) {
    .C("remove_all_mwss_c")
    return(library.dynam.unload("isismdl", libpath))
}
