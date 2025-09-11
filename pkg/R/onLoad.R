#' @useDynLib isismdl, .registration = TRUE,  .fixes = "C_"
.onLoad <- function(libname, pkgname) {
  # initialise Fortran modules
  .C(C_init_modules_c)
}
