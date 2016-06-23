#' @useDynLib macromod init_modules
.onAttach <- function(libname, pkgname) {
    # initialise Fortran modules
    .Fortran("init_modules")
}
