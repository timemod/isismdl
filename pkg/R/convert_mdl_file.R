#' Converts an \code{\link{IsisMdl}} a model file.
#'
#' @param model_file The name of the model file.
#' An extension \code{mdl} is appended to the specified name if the filename
#' does not already have an extension
#' @param output_file The name of the output file.
#' @param conversion_options conversion options. See section Conversion options.
#' @param parse_options a named list with options passed to the model parser.
#' See section "Parse option" in the description of function
#' \code{\link{isis_mdl}}.
#' @useDynLib isismdl convert_mdl_file_c
#' @export
#'
#' @section Conversion options:
#'
#' The following conversion options can be specified with argument
#' \code{conversion_options}. This argument should be a named list
#' (for example, \code{list(substitute = TRUE)}).
#' \describe{
#' \item{\code{substitute}}{Specify \code{TRUE} to substitute user functions.
#' The default is \code{FALSE}.}
#' \item{\code{make_dynare}}{Specify \code{TRUE} to convert the model
#' to a Dynare mod file.The default is \code{FALSE}.}
#' }
convert_mdl_file <- function(model_file, output_file,
                             conversion_options = list(),
                             parse_options = list()) {

  default_parse_options <- list(flags = NULL,
                                include_dirs = NULL,
                                gen_dep_file = FALSE)

  if (!missing(conversion_options) && !is.list(conversion_options)) {
    stop("Argument conversion_options is not a list")
  }

  parse_options_ <- default_parse_options
  if (!missing(parse_options)) {
    names <- names(parse_options)
    parse_options_[names] <- parse_options
  }

  with(parse_options_, {
    retval <- .Call(convert_mdl_file_c, model_file, output_file, flags,
                    include_dirs, conversion_options)
    return(retval)
  })


}
