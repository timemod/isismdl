#' Returns an IFN model

#' This function returns an uninitialized IFN model.
#' @return a \code{\link{IsisMdl}} object
#' @examples
#' mdl <- ifn_mdl()
#' @export
ifn_mdl <- function() {
  mdl_file <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  mdl_file_orig <- system.file("models", "ifn.mdl", package = "isismdl")
  file.copy(mdl_file_orig, mdl_file)
  retval <- isis_mdl(mdl_file)
  unlink(mdl_file)

  # remove mrf file
  base_name <- file_path_sans_ext(mdl_file)
  mrf_file <- paste(base_name, "mrf", sep = ".")
  unlink(mrf_file)

  return(retval)
}
