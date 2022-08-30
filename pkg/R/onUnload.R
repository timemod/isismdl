.onUnload <- function(libpath) {
    return(library.dynam.unload("isismdl", libpath))
}
