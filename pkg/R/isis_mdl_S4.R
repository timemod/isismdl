#' Creates an \code{\link{IsisMdlS4}} object from a model file.
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
#' @param modelname The name of the model file.
#' An extension \code{mdl} is appended to the specified name if the filename
#' does not already have an extension.
#' @useDynLib isismdl compile_mdl_c
#' @examples
#' copy_example_mdl("islm")
#' mdl <- isis_mdl("islm.mdl")
#' \dontshow{
#' unlink("islm.*")
#' }
#' @seealso \code{\link{copy_example_mdl}}, \code{\link{IsisMdl}} and
#' \code{\link{IsisMdl}}
#' @importFrom tools file_path_sans_ext
#' @export
isis_mdl_S4 <- function(modelname) {
    # TODO: currently, compile_mdl_c generates a mif file
    # that is later read by read_mdl_c. This can be simpler:
    # after compilation the model information can be put
    # directly in Fortran memory. Then there is no need to
    # write and read the mif file.
    retval <- .Call(compile_mdl_c, modelname)
    if (!retval) {
        stop("Compilation was not succesfull")
    }
    base_name <- file_path_sans_ext(modelname)
    mif_file <- paste(base_name, "mif", sep = ".")

    model_index <- .Call(read_mdl_c, mif_file)

    # get maximum lag and lead
    ret <- .Fortran("get_max_lag_lead_fortran", model_index = model_index,
                    maxlag = 1L, maxlead = 1L)
    maxlag <- ret$maxlag
    maxlead <- ret$maxlead

    # get parameters
    names <- .Call(get_par_names_c, model_index)
    params <- .Call("get_param_c", model_index = model_index,
                    names = names)

    unlink(mif_file)
    return(IsisMdlS4(model_index = model_index, maxlag = maxlag,
                     maxlead = maxlead, params = params))
}
