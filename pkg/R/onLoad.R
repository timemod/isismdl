#' @useDynLib isismdl init_modules_c
.onLoad <- function(libname, pkgname) {
    # initialise Fortran modules
    .C("init_modules_c")
}
