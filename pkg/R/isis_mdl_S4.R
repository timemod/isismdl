#' Creates an \code{\link{IsisMdlS4}} object from a model file.
#'
#' This function creates an \code{\link{IsisMdlS4}} object.
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
#' @param period a \code{\link[regts]{regperiod_range}} object
#' @param data the model data as a  \code{\link[regts]{regts}} object with column
#' names
#' @param ca the constant adjustments as a  \code{\link[regts]{regts}} object
#' with column names
#' @param fix_values the fix values as a  \code{\link[regts]{regts}} object
#' with column names
#' @param fit_targets the fit targets as a  \code{\link[regts]{regts}} object
#' with column names
#' @useDynLib isismdl compile_mdl_c
#' @examples
#' copy_example_mdl("islm")
#' mdl <- isis_mdl_S4("islm.mdl")
#' \dontshow{
#' unlink("islm.*")
#' }
#' @seealso \code{\link{copy_example_mdl}} and \code{\link{IsisMdlS4}}
#' @importFrom tools file_path_sans_ext
#' @useDynLib isismdl get_solve_opts_c
#' @useDynLib isismdl get_cvgcrit_c
#' @useDynLib isismdl get_ftrelax_c
#' @export
isis_mdl_S4 <- function(modelname, period, data = NULL, ca = NULL,
                        fix_values = NULL, fit_targets = NULL) {
    # TODO: currently, compile_mdl_c generates a mif file
    # that is later read by read_mdl_c. This can be simpler:
    # after compilation the model information can be put
    # directly in Fortran memory. Then there is no need to
    # write and read the mif file.

    period <- as.regperiod_range(period)

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
    par_names <- .Call(get_par_names_c, model_index)
    params <- .Call("get_param_c", model_index = model_index,
                    names = par_names)

    names    <- .Call(get_var_names_c, "all",     model_index)
    ca_names <- .Call(get_var_names_c, "allfrml", model_index)
    solve_opts <- .Call(get_solve_opts_c, model_index)

    cvgcrit = .Call("get_cvgcrit_c", model_index, 0L)
    names(cvgcrit) <- names

    endo_lead_names <- .Call(get_var_names_c, "all_endolead", model_index)
    if (length(endo_lead_names) > 0) {
        ftrelax = .Call("get_ftrelax_c", model_index)
        names(ftrelax) <- endo_lead_names
    } else {
        ftrelax <- NULL
    }

    unlink(mif_file)

    data_period <- regperiod_range(
        start_period(period) - maxlag, end_period(period) + maxlead)

    init_data <- create_data(names, data_period)
    init_ca   <- create_ca(ca_names, period)

    mdl <- IsisMdlS4(control = ModelControl$new(model_index),
                     maxlag = maxlag, maxlead = maxlead, params = params,
                     solve_opts = solve_opts,
                     cvgcrit = cvgcrit, ftrelax = ftrelax,
                     names = names, ca_names = ca_names,
                     period = period, data_period = data_period,
                     data = init_data, ca = init_ca)

    # update data
    if (!missing(data)) {
        if (!is.null(colnames(data))) {
            mdl <- set_data(mdl, data, colnames(data))
        } else {
            stop("data has no column names")
        }
    }
    if (!missing(ca)) {
        if (!is.null(colnames(ca))) {
            mdl <- set_ca(mdl, ca, colnames(ca))
        } else {
            stop("ca has no column names")
        }
    }
    if (!missing(fix_values)) {
        if (!is.null(colnames(fix_values))) {
            mdl <- set_fix(mdl, fix_values, colnames(fix_values))
        } else {
            stop("fix_values has no column names")
        }
    }
    if (!missing(fit_targets)) {
        if (!is.null(colnames(fit_targets))) {
            mdl <- set_fit(mdl, fit_targets, colnames(fit_targets))
        } else {
            stop("fit_targets has no column names")
        }
    }

    return(mdl)
}
