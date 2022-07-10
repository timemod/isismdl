#' Creates an \code{\link{IsisMdl}} object from a model file.
#'
#' This function creates an \code{\link{IsisMdl}} object.
#' A model as defined on an external ASCII file is parsed, analyzed and
#' converted into an internal code. This internal code is used to evaluate
#' the model equations.
#'
#' @details
#'
#' The file containing the model must have an extension \code{mdl}.
#'
#' Some technical information about the model and a
#' cross reference of the model is written
#' to an external file with extension \code{mrf}.
#' For each variable its maximum lag and lead are given and a list
#' of equations (by name) in which it occurs.
#'
#' The parser also orders the equations of the model into three
#' separate blocks.
#'
#' \itemize{
#' \item
#' the \emph{pre-recursive} block  containing equations
#' which can be solved recursively from exogenous and lagged
#' variables only.
#' \item
#' the \emph{simultaneous} block containing all equations
#' with interdependent endogenous variables.
#' \item
#' the \emph{post-recursive} block containing equations
#' which can be solved recursively once the two previous blocks
#' have been solved.
#' }
#' The ordering process also provides a list of so-called feedback
#' variables, i.e. variables whose value must be assumed known to
#' make the \emph{simultaneous} block recursive.
#' Initial guesses for these variables must be provided in order to
#' solve a model.
#' If a model has no feedback variables, it is a recursive model (it
#' can be solved in one pass through the equations).
#'
#' If the parser encounters errors in the model, these are written
#' to a file with an extension \code{err}.
#' All generated files have the same base name as the model file.
#'
#' @param model_file The name of the model file.
#' An extension \code{mdl} is appended to the specified name if the filename
#' does not already have an extension
#' @param period a \code{\link[regts]{period_range}} object
#' @param data the model data as a  \code{\link[regts]{regts}} object with column
#' names
#' @param ca the constant adjustments as a  \code{\link[regts]{regts}} object
#' with column names
#' @param fix_values the fix values as a  \code{\link[regts]{regts}} object
#' with column names
#' @param parse_options a named list with options passed to the model parser.
#' See section "Parse options"
#' @param silent A logical (default \code{FALSE}). If \code{TRUE}, then
#' output of the model parser is suppressed.
#' @useDynLib isismdl compile_mdl_c
#'
#' @section Parse options:
#'
#' The following parse options can be specified with argument
#' \code{parse_options}, which should be a named list
#' \describe{
#' \item{\code{"flags"}}{A character vector with the flags for conditional
#' compilation. Consult Section 3.11.2 "Conditional compilation" in the Isis reference
#' manual for more information about conditional compilation}
#' \item{\code{"include_dirs"}}{A vector with the names of directories
#' that should be added to the list of
#'  directories used to searched for include files.
#'  In the model file, the \code{\#include} directive (see Section
#'  3.11.1 "File inclusion" in the Isis Reference Manual) is used
#' to include another file in the model. The specified name of the include
#' file can be a relative or absolute path. If the path is relative, then
#' the model searches for the include file.
#' It first searches in the same directory
#' where the source file is located. If not found there,
#' then the compiler searches in the directories specified with
#' argument \code{include_dir}, in the order that the directories
#' have been specified.
#' If the include file is still not found, the parser
#' searches in the current directory}
#' }
#' @examples
#'
#' # copy the islm.mdl file in the directory models of the package
#' # directory to the current directory
#' mdl_file <- system.file("models", "islm.mdl", package = "isismdl")
#' file.copy(mdl_file, "islm.mdl")
#'
#' mdl <- isis_mdl("islm.mdl")

#'
#' # an example with parse option "include_dirs":
#' mdl <- isis_mdl("islm.mdl", parse_options = list(include_dirs = "mdlincl"))
#'
#' \dontshow{
#' unlink("islm.*")
#' }
#' @seealso \code{\link{IsisMdl}}, \code{\link{islm_mdl}} and
#' \code{\link{ifn_mdl}}
#' @importFrom tools file_path_sans_ext
#' @importFrom regts range_union
#' @importFrom regts as.period_range
#' @importFrom regts start_period
#' @importFrom regts end_period
#' @importFrom readr read_file
#' @importFrom tools file_ext
#' @importFrom utils capture.output
#' @export
isis_mdl <- function(model_file, period, data, ca, fix_values,
                     parse_options, silent = FALSE) {

  model_file <- check_mdl_file(model_file)

  if (!missing(period)) {
    period <- as.period_range(period)
  }

  parse_options <- check_parse_options(parse_options)

  # compile_mdl_c writes intermediate results to a so called
  # mif file (model information file). These results are then
  # read by read_model. This strange situation is due to the history
  # of package isismdl. Changing this behavior is not trivial and requires a
  # significant reorganization of the code.
  mif_file <- tempfile(pattern = "isismdl_", fileext = ".mif")
  preproc_file <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  flags <- parse_options$flags
  include_dirs <- parse_options$include_dirs

  call_compile_mdl_c <- function() {
    return(.Call(compile_mdl_c, model_file, mif_file, preproc_file,
                 flags, include_dirs))
  }
  if (silent) {
    output <- capture.output({
      retval <- call_compile_mdl_c()
    })
  } else {
    retval <- call_compile_mdl_c()
  }

  if (!retval) {
    stop("Compilation was not successful")
  }

  model_text <- trimws(read_file(preproc_file), which = "right")
  ok <- file.remove(preproc_file)

  mdl <- IsisMdl$new(mif_file = mif_file, model_text = model_text,
                     silent = silent)
  unlink(mif_file)

  if (!missing(data)) {
    data_period <- get_period_range(data)
    if (!missing(period)) {
      # data_period should be the union of the period_range of data
      # and the supplied period extended with a lag and lead period.
      data_period_2 <- period_range(
        start_period(period) - mdl$get_maxlag(),
        end_period(period)   + mdl$get_maxlead())
      data_period <- range_union(data_period, data_period_2)
    }
    if (is.null(colnames(data))) {
      stop("data has no column names")
    } else {
      mdl$init_data(data_period = data_period, data = data)
    }
  }

  if (!missing(period)) {
    mdl$set_period(period)
  }

  if (!missing(ca)) {
    if (!is.null(colnames(ca))) {
      mdl$set_ca(ca, colnames(ca))
    } else {
      stop("ca has no column names")
    }
  }
  if (!missing(fix_values)) {
    if (!is.null(colnames(fix_values))) {
      mdl$set_fix(fix_values, colnames(fix_values))
    } else {
      stop("fix_values has no column names")
    }
  }
  return(mdl)
}
