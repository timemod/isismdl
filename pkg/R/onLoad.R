#' @useDynLib isismdl init_modules
.onLoad <- function(libname, pkgname) {
    # initialise Fortran modules
    .Fortran("init_modules")
}
