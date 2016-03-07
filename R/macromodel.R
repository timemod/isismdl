#' "regperiod_range" class
#'
#' @name regperiod_range-class
#' @exportClass regperiod_range
setOldClass("regperiod_range")


# TODO: maybe a R6 class is better suited? Check this out.


#' A reference class to a macroecomic model
#'
#' @useDynLib macromod get_variable_names_c
#' @useDynLib macromod set_period_fortran
#' @useDynLib macromod get_data_c
#' @useDynLib macromod set_c
#' @useDynLib macromod set_rms_c
#' @useDynLib macromod run_equations_fortran
#' @useDynLib macromod solve_c
#' @import regts
#' @import methods
#' @field model_period the model period
MacroModel <- setRefClass("MacroModel",
    fields = list(model_index = "integer",
                  model_period = "regperiod_range"),
    methods = list(
        # TODO: add finalize function, and clear the memory
        get_variable_names = function() {
            "Returns the names of the model variables"
            return (.Call(get_variable_names_c, as.integer(model_index)))
        },
        set_period = function(period) {
            "Sets the model period"
            period <- as.regperiod_range(period)
            model_period <<- period
            retval <- .Fortran("set_period_fortran", model_index = model_index,
                               start = as.integer(period$start),
                              end = as.integer(period$end),  freq = as.integer(period$freq),
                              ier = as.integer(1))
            return (invisible(NULL))
        },
        get_data = function() {
            "Returns the model data"
            data <- .Call(get_data_c, model_index)
            p1 <- get_start_period(model_period) + data[[2]] - 1
            retval <- ts(data[[1]], start = p1$data, frequency = p1$freq)
            return (as.regts(retval))
        },
        set_data = function(ts_data) {
            "Sets the model data"
            return (set_var(as.integer(1), model_index, ts_data, model_period))
        },
        set_ca = function(ts_data) {
            "Sets the constant adjustments"
            return (set_var(as.integer(2), model_index, ts_data, model_period))
        },
        set_fix = function(ts_data) {
            "Sets the fix data"
            return (set_var(as.integer(3), model_index, ts_data, model_period))
        },
        set_fit = function(ts_data) {
            "Sets the fit data"
            return (set_var(as.integer(4), model_index, ts_data, model_period))
        },
        set_rms = function(rms_list) {
            "Sets the rms values"
            return (invisible(.Call(set_rms_c, model_index, rms_list)))
        },
        solve = function(period) {
            "Solves the model for the specified period"
            js <- get_period_indices(period, model_period)
            retval <- .Call("solve_c", model_index = model_index,
                               jtb = js$startp, jte = js$endp,
                               solve_period = as.character(period))
            class(retval) <- "solve_report"
            return (retval)
        }
    )
)

get_period_indices <- function(period, model_period) {
    period <- as.regperiod_range(period)
    mdl_period_start <- get_start_period(model_period)
    startp <- as.integer(get_start_period(period) - mdl_period_start + 1)
    endp <- as.integer(get_end_period(period) - mdl_period_start + 1)
    return (list(startp = startp, endp = endp))
}

# general function used to update model data, constant adjustments,
# fix values or fit targets. Should not be used by user and should
# therefore not be documented and exported.
set_var <- function(set_type, model_index, ts_data, model_period) {
    ts_data <- as.regts(ts_data)
    shift <- get_period_indices(get_regperiod_range(ts_data), model_period)$startp
    if (is.matrix(ts_data) && is.integer(ts_data)) {
        # make sure that data is a matrix of real values and no integers
        old_colnames <- colnames(ts_data)
        data <- matrix(as.double(ts_data), dim(ts_data))
        colnames(ts_data) <- old_colnames
    } else if (!is.matrix(ts_data)) {
        stop("macromodel cannot handle univariate timeseries yet")
    }
    return (invisible(.Call(set_c, set_type, model_index, ts_data, shift)))
}

