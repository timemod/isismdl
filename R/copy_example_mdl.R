#' Create a copy of an example model file
#'
#' The package \code{isismdl} includes a number of example model files
#' that can be used to get started with this package. These files are
#' located in the directory \code{models} of the package directory. This
#' function can be used to copy the model file from this directory to
#' a specified directory
#' @param model_name the name of the model (currently only \code{"islm"} is
#' supported)
#' @param filename the file name of the copy model file. By default, this
#' is \code{model_name} suffixed with extension \code{.mdl}.
#' @return \code{TRUE} is the copy was succesful
#' @examples
#' copy_example_mdl("islm")
#' \dontshow{
#' dir.create("mdl")
#' }
#' copy_example_mdl("islm", filename = "mdl/islm.mdl")
#' \dontshow{
#' unlink("islm.*")
#' unlink("mdl", recursive = TRUE)
#' }
#' @seealso \code{\link{compile_mdl}} and \code{\link{IsisMdl}}
#' @export
copy_example_mdl <- function(model_name,
                             filename = paste(model_name, "mdl", sep = ".")) {
    if (!model_name %in% "islm") {
        stop(paste("Model", model_name, "is not an example model file"))
    }
    mdl_file <- system.file("models", paste(model_name, "mdl", sep = "."),
                            package = "isismdl")
    file.copy(mdl_file, filename, overwrite = TRUE)
}
