#' "regperiod_range" class
#'
#' @name regperiod_range-class
#' @exportClass regperiod_range
setOldClass("regperiod_range")


# TODO: maybe a R6 class is better suited? The only problem with R6 is that
# roxygen does not seem to work yet for R6 classes.

#' A reference class representing a macroecomic model
#'
#' @useDynLib macromod get_names_c
#' @useDynLib macromod set_period_fortran
#' @useDynLib macromod get_data_c
#' @useDynLib macromod get_fix_fit_c
#' @useDynLib macromod set_c
#' @useDynLib macromod set_rms_c
#' @useDynLib macromod run_equations_fortran
#' @useDynLib macromod solve_c
#' @useDynLib macromod filmdt_c
#' @useDynLib macromod remove_mws_fortran
#' @import regts
#' @import methods
#' @field maxlag the maximum lag of the model
#' @field maxlead the maximum lead of the model
#' @field model_period the model period. This is the maximum period for which
#' the model will be solved.
#' @field model_data_period the model data period, i.e. the model
#' period extended wih the lag period and lead period. For example, suppose that the
#' model has a maximum lag of 2 and a maximum lead of 1. If the model period is
#' \code{"2015Q3/2016Q2"}, then the model data period is \code{"2015Q1/2016Q3"}.
MacroModel <- setRefClass("MacroModel",
    fields = list(model_index = "integer",
                  maxlag = "integer",
                  maxlead = "integer",
                  model_period = "regperiod_range",
                  model_data_period = "regperiod_range"),
    methods = list(
        finalize = function() {
            .Fortran("remove_mws_fortran", model_index = model_index)
        },
        get_variable_names = function() {
            "Returns the names of the model variables"
            return (sort(.Call(get_names_c, "data", as.integer(model_index))))
        },
        get_ca_names = function() {
            "Returns the names of the constant adjustments"
            # note: the names returned  by get_ca_names are not sorted alphabetically,
            # therefore sort explicitly
            return (sort(.Call(get_names_c, "ca", as.integer(model_index))))
        },
        set_period = function(period) {
            "Sets the model period. This is the longest period for which
             the model will be solved. This method also allocates storage for
             all model timeseries and constant adjustments. Model timeseries are
             available for the so called 'model data period', which is
             the model period extended with a lag and lead period. Constant
             adjustments are only available for the model period. This method
             also initialises all model timeseries with \\code{NA} and all constant
             adjusments with 0."
            period <- as.regperiod_range(period)
            model_period <<- period
            retval <- .Fortran("set_period_fortran", model_index = model_index,
                               start = as.integer(period$start),
                               end = as.integer(period$end),
                               freq = as.integer(period$freq),
                               ier = as.integer(1))

            model_data_period <<- regperiod_range(
                                         get_start_period(model_period) - maxlag,
                                         get_end_period(model_period) + maxlead)
            return (invisible(NULL))
        },
        get_data = function(names = get_variable_names(), period = model_data_period) {
            "Returns the model data"
            if (!missing(names)) {
                names <- intersect(names, get_variable_names())
            }
            return (get_variables(.self, "data", names, period))
        },
        get_ca = function(names = get_ca_names(), period = model_period) {
            "Returns the constant adjustments"
            if (!missing(names)) {
                names <- intersect(names, get_ca_names())
            }
            return (get_variables(.self, "ca", names, period))
        },
        get_fix = function() {
            "Returns the fix values"
            return (get_fix_fit(.self, type = "fix"))
        },
        get_fit = function() {
            "Returns the fit targets"
            return (get_fix_fit(.self, type = "fit"))
        },
        set_data = function(ts_data) {
            "Sets the model data"
            return (set_var(.self, as.integer(1), ts_data))
        },
        set_ca = function(ts_data) {
            "Sets the constant adjustments"
            return (set_var(.self, as.integer(2), ts_data))
        },
        set_fix = function(ts_data) {
            "Sets the fix data"
            return (set_var(.self, as.integer(3), ts_data))
        },
        set_fit = function(ts_data) {
            "Sets the fit data"
            return (set_var(.self, as.integer(4), ts_data))
        },
        set_rms = function(rms_list) {
            "Sets the rms values"
            return (invisible(.Call(set_rms_c, model_index, rms_list)))
        },
        solve = function(period = model_period) {
            "Solves the model for the specified period"
            check_period_set(.self)
            js <- get_period_indices(period, model_period)
            retval <- .Call("solve_c", model_index = model_index,
                               jtb = js$startp, jte = js$endp,
                               solve_period = as.character(period))
            class(retval) <- "solve_report"
            return (retval)
        },
        fill_mdl_data = function(period = model_data_period) {
            "Calculates missing model data from identities."
            check_period_set(.self)
            js <- get_period_indices(period, model_period)
            retval <- .Call("filmdt_c", model_index = model_index,
                            jtb = js$startp, jte = js$endp,
                            solve_period = as.character(period))
            class(retval) <- "filmdt_report"
            return (retval)
        },
        get_mws = function() {
            "Returns an mws object"
            check_period_set(.self)

            data <- get_data()
            ca   <- get_ca()

            # remove columns /rows with only NA from data
            # todo: skip leading/trailing rows with only NA
            data <- data[ , ! apply(is.na(data) , 2 , all)]

            # remove columns with only 0 from ca
            # todo: skip leading/trailing columns with only NA
            ca <- ca[, !apply(ca == 0, 2, all)]

            # todo: rms values

            return (structure(list(var_names = get_variable_names(),
                                   ca_names = get_ca_names(),
                                   model_period = model_period,
                                   data = data, ca = ca,
                                   fix = get_fix(),
                                   fit = get_fit()),
                              class="mws"))
        },
        set_mws = function(x) {
            "Sets the model workspace"
            if (!inherits(x, "mws")) {
                stop("x is not an mws object")
            }
            if (!identical(x$var_names, get_variable_names()) |
                !identical(x$ca_names, get_ca_names())) {
                    stop("Mws x does not agree with the model definition")
            }
            set_period(x$model_period)
            set_data(x$data)
            set_ca(x$ca)
            if (!is.null(x$fix)) {
                set_fix(x$fix)
            }
            if (!is.null(x$fit)) {
                set_fit(x$fit)
            }
        }
    )
)

get_period_indices <- function(period, model_period, extended = TRUE) {
    period <- as.regperiod_range(period)
    mdl_period_start <- get_start_period(model_period)
    startp <- as.integer(get_start_period(period) - mdl_period_start + 1)
    endp <- as.integer(get_end_period(period) - mdl_period_start + 1)
    return (list(startp = startp, endp = endp))
}

# Check if the model_period has been set
check_period_set = function(x) {
    # Check if the model_period has been set
    if (is.null(x$model_period)) {
        stop("The model period is not set. Set the model period with set_mdl_period()")
    }
    return (NULL)
}

# general function used to update model data, constant adjustments,
# fix values or fit targets. Should not be used by user and should
# therefore not be documented and exported.
set_var = function(x, set_type, ts_data) {
    check_period_set(x)
    ts_data <- as.regts(ts_data)
    shift <- get_period_indices(get_regperiod_range(ts_data), x$model_period)$startp
    if (is.matrix(ts_data) && is.integer(ts_data)) {
        # make sure that data is a matrix of real values and no integers
        old_colnames <- colnames(ts_data)
        data <- matrix(as.double(ts_data), dim(ts_data))
        colnames(ts_data) <- old_colnames
    } else if (!is.matrix(ts_data)) {
        stop("macromodel cannot handle univariate timeseries yet")
    }
    return (invisible(.Call(set_c, set_type, x$model_index, ts_data, shift)))
}

# general function used to get model data or constant adjustments
get_variables <- function(x, type, names, period) {
    check_period_set(x)
    period <- as.regperiod_range(period)
    if (length(names) > 0) {
        js <- get_period_indices(period, x$model_period)
        data <- .Call("get_data_c", type = type, model_index = x$model_index,
                      names = names, jtb = js$startp, jte = js$endp)
        return (regts(data, start = get_start_period(period), names = names))
    } else {
        return (NULL)
    }
}

# general function for getting fix or fit values
get_fix_fit <- function(mdl, type) {
    ret <- .Call("get_fix_fit_c", type = type, model_index = mdl$model_index)
    if (!is.null(ret)) {
        ret <- regts(ret[[2]], start = get_start_period(mdl$model_period) + ret[[1]] - 1, names = ret[[3]])
        ret <- ret[, sort(colnames(ret))]
    }
    return (ret)
}


