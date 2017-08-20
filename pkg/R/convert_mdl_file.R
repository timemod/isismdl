#' Converts an \code{\link{IsisMdl}} a model file.
#'
#' @param model_file The name of the model file.
#' An extension \code{mdl} is appended to the specified name if the filename
#' does not already have an extension
#' @param conversion_options conversion options. See details.
#' @param compile_options options passed to the compiler. See details.
#' @useDynLib isismdl convert_mdl_file_c
#' @export
convert_mdl_file <- function(model_file, conversion_options = list(),
                             compile_options = list()) {

  default_compile_options <- list(flags = NULL,
                                  include_dirs = NULL,
                                  gen_dep_file = FALSE)

  if (!missing(conversion_options) && !is.list(conversion_options)) {
    stop("Argument conversion_options is not a list")
  }

  compile_options_ <- default_compile_options
  if (!missing(compile_options)) {
    names <- names(compile_options)
    compile_options_[names] <- compile_options
  }

  with(compile_options_, {
    retval <- .Call(convert_mdl_file_c, model_file, flags, include_dirs,
                    conversion_options)
    return(retval)
  })


}
