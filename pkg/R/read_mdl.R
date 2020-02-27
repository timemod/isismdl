#' Reads an \code{IsisMdl} object from a file
#'
#' This function reads a model from a file that has been written
#' by \code{\link{IsisMdl}} method \code{\link{write_mdl}}
#'
#' \code{read_mdl} employs the serialization interface provided
#' by base R function \code{\link[base]{readRDS}}.
#'
#' @param file filename (typically with extension \code{.ismdl})
#' @param silent A logical (default \code{FALSE}). If \code{TRUE}, then
#' output is suppressed.
#' @return an \code{\link{IsisMdl}} object
#' @examples
#' mdl <- islm_mdl("2017Q1/2019Q2")
#' mdl$write_mdl("islm_mdl.ismdl")
#' mdl2 <- read_mdl("islm_mdl.ismdl")
#' @seealso \code{\link{write_mdl}}
#' @export
read_mdl <- function(file, silent = FALSE) {
  serialized_mdl <- readRDS(file)
  return(IsisMdl$new(serialized_mdl, silent = silent))
}
