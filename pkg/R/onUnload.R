.onUnload <- function(libpath) {
    .C(C_remove_all_mwss_c)
    return(library.dynam.unload("isismdl", libpath))
}
