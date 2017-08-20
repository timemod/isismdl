#' Creates an \code{\link{IsisMdl}} object from a model file.
#'
#' This function creates an \code{\link{IsisMdl}} object.
#' A model as defined on an external ASCII file is analysed and
#' compiled into an internal code. This internal model code is
#' written to a file with extension \code{mif} containing equation
#' and variable information.
#'
#' @details
#'
#' The file containing the model must have an extension \code{mdl}.
#' The compiled model is \emph{not} kept in memory. The function
#' \code{\link{read_mdl}} should be called to load the model for immediate
#' use.
#'
#' In addition, some technical information about the model and a
#' cross reference of the model is written
#' to an external file with extension \code{mrf}.
#' For each variable its maximum lag and lead are given and a list
#' of equations (by name) in which it occurs.
#'
#' The compiler also orders the equations of the model into three
#' separate blocks
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
#' If the compiler encounters errors in the model, these are written
#' to a file with an extension \code{err}.
#' All generated files have the same basename as the model file.
#'
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
#' @param compile_options options passed to the compiler. See details.
#' @useDynLib isismdl compile_mdl_c
#' @examples
#'
#' # copy the islm.mdl file in the directory models of the package
#' # directory to the current directory
#' mdl_file <- system.file("models", "islm.mdl", package = "isismdl")
#' file.copy(mdl_file, "islm.mdl")
#'
#' mdl <- isis_mdl("islm.mdl")
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
#' @export
isis_mdl <- function(model_file, period, data, ca, fix_values,
                     compile_options) {

  if (!missing(period)) {
    period <- as.period_range(period)
  }


  default_compile_options <- list(flags = NULL,
                                  include_dirs = NULL,
                                  gen_dep_file = FALSE)

  compile_options_ <- default_compile_options
  if (!missing(compile_options)) {
    names <- names(compile_options)
    compile_options_[names] <- compile_options
  }

  # TODO: currently, compile_mdl_c generates a mif file
  # that is later read by read_mdl_c. This can be simpler:
  # after compilation the model information can be put
  # directly in Fortran memory. Then there is no need to
  # write and read the mif file
  with(compile_options_, {
    retval <- .Call(compile_mdl_c, model_file, flags, include_dirs,
                    gen_dep_file)
    if (!retval) {
      stop("Compilation was not succesfull")
    }
  })

  base_name <- file_path_sans_ext(model_file)
  mif_file <- paste(base_name, "mif", sep = ".")
  mdl <- IsisMdl$new(mif_file = mif_file)
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
