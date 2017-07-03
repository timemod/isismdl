#' Reads a model from an RDS file
#'
#' This function reads a model from an RDS file that has been written
#' by method \code{\link{write_mdl}} of an \code{\link{IsisMdl}} object.
#' @param file the name of the RDS file
#' @return a \code{\link{IsisMdl}} object
#' @examples
#' mdl <- islm_mdl("2017Q1/2019Q2")
#' mdl$write_mdl("islm_mdl.rds")
#' mdl2 <- read_mdl("islm_mdl.rds")
#' @seealso \code{\link{write_mdl}}
#' @export
read_mdl <- function(file) {
  serialized_mdl <- readRDS(file)
  return(IsisMdl$new(serialized_mdl))
}
