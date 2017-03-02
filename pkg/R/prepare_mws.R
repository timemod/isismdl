# Prepare the model workspace in the Fortran modules. This function
# should be called before solving the model,  filling model data, etc.
# INPUT:
#   mdl    the IsisMdl object
#   period the model period IN FORTRAN MEMORY (could be
#              different from mdl@period!)
#   solve  a logical. TRUE if the mws is prepared for solving,
#          FALSE otherwise
#' @useDynLib isismdl activate_all_equations
#' @useDynLib isismdl set_eq_status_c
#' @useDynLib isismdl set_solve_opts_c
#' @useDynLib isismdl set_cvgcrit_c_S4
#' @useDynLib isismdl set_ftrelax_c_S4
#' @useDynLib isismdl set_rms_c
prepare_mws <- function(mdl, period, solve = TRUE) {

    set_param_mws(mdl)

    .Call("set_solve_opts_c", mdl@control$index, mdl@solve_opts)
    .Call("set_cvgcrit_c_S4", mdl@control$index, mdl@cvgcrit)
    if (!is.null(mdl@ftrelax)) {
        .Call("set_ftrelax_c_S4", mdl@control$index, mdl@ftrelax)
    }

    # handle inactive / active equations
    .Call("activate_all_equations", mdl@control$index)
    if (length(mdl@inactive_eqs) > 0) {
        .Call("set_eq_status_c", mdl@control$index, mdl@inactive_eqs,
              "inactive")
    }

    set_period(mdl, period)
    set_var(mdl, 1L,     mdl@data, period)
    if (!is.null(mdl@ca)) {
        set_var(mdl, 2L, mdl@ca, period)
    }
    if (!is.null(mdl@fix)) {
        t <- system.time(
        set_var(mdl, 3L, mdl@fix, period)
        )
    }
    if (solve && !is.null(mdl@fit)) {
        set_var(mdl, 4L, mdl@fit, period)
    }

    if (!is.null(mdl@rms)) {
        .Call("set_rms_c", mdl@control$index, mdl@rms)
    }
    return(invisible(NULL))
}

set_period <- function(mdl, period) {
    p1 <- start_period(period)
    p2 <- end_period(period)
    start <- as.integer(c(get_year(p1), get_subperiod(p1)))
    end   <- as.integer(c(get_year(p2), get_subperiod(p2)))
    retval <- .Fortran("set_period_fortran", model_index = mdl@control$index,
                       start = start, end = end, freq  = as.integer(period[3]),
                       ier = 1L)
    return(invisible(NULL))
}

# General function used to update model data, constant adjustments,
# fix values or fit targets in Fortran memory.
# INPUT:
#   mdl        the IsisMdl object
#   set_type   an integer specifying the type of data.
#              (1 = data, 2 = ca, 3 = fix values, 4 = fit targets)
#   data       the data (a regts or ts object)
#   mdl_period the model period IN FORTRAN MEMORY (this can be
#              different from mdl@period!
set_var <- function(mdl, set_type, data, mdl_period) {
    shift <- get_period_indices(mdl_period, get_regperiod_range(data))$startp
    names <- colnames(data)
    .Call(set_c, set_type, mdl@control$index, data, names, shift)
    return(invisible(NULL))
}

# Transfer the model parameters to the mws
set_param_mws <- function(mdl) {

    if (!is.list(mdl@params)) {
        stop("mdl@params is not a list")
    }

    # convert integer list elements to numeric
    p <- lapply(mdl@params, as.numeric)

    nset <- .Call("set_param_c", model_index = mdl@control$index, p)
    return(invisible(NULL))
}
