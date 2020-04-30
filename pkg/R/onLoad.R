#' @useDynLib isismdl init_modules
.onLoad <- function(libname, pkgname) {
    # initialise Fortran modules
    .C("init_modules")
}
