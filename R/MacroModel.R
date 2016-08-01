library(R6)

#' "regperiod_range" class
#'
#' @name regperiod_range-class
#' @exportClass regperiod_range
setOldClass("regperiod_range")

#' An R6 class class representing a macroecomic model
#'
#' @docType class
#' @importFrom R6 R6Class
#' @useDynLib macromod read_mdl_c
#' @useDynLib macromod get_max_lag_lead_fortran
#' @useDynLib macromod get_names_c
#' @useDynLib macromod set_period_fortran
#' @useDynLib macromod get_data_c
#' @useDynLib macromod get_fix_fit_c
#' @useDynLib macromod set_c
#' @useDynLib macromod set_rms_c
#' @useDynLib macromod run_equations_fortran
#' @useDynLib macromod set_solve_opts_c
#' @useDynLib macromod get_solve_opts_c
#' @useDynLib macromod solve_c
#' @useDynLib macromod filmdt_c
#' @useDynLib macromod remove_mws_fortran
#' @import regts
#' @importFrom "methods" "new"
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} representing a macroeconomic model.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # copy islm model and compile
#' copy_example_mdl("islm")
#' compile_mdl("islm.mdl")
#'
#' islm_model <- MacroModel$new("islm.mif")
#' islm_model$set_period("2010Q3/2011Q4")
#' islm_model$set_data(islm_input)
#' islm_model$solve()
#' \dontshow{
#' unlink("islm.*")
#' }
#' @field maxlag the maximum lag of the model
#' @field maxlead the maximum lead of the model
#' @field model_period the model period. This is the maximum period for which
#' the model will be solved.
#' @field model_data_period the model data period, i.e. the model
#' period extended wih the lag period and lead period. For example, suppose that
#' the model has a maximum lag of 2 and a maximum lead of 1. If the model period
#'  is \code{"2015Q3/2016Q2"}, then the model data period is \code{"2015Q1/2016Q3"}.
#' @section Methods:
#' \describe{
#' \item{\code{get_variable_names()}}{Returns the names of the model variables}
#' \item{\code{get_ca_names()}}{Returns the names of the constant adjustments}
#' \item{\code{set_period(period)}}{Sets the model period. \code{period}
#' is a \code{\link{regperiod_range}} object or an object that can be coerced
#' to a \code{regperiod_range}. The model period is the longest period for which
#' the model may be solved. This method also allocates storage for
#' all model timeseries and constant adjustments. Model timeseries are
#' available for the so called 'model data period', which is
#' the model period extended with a lag and lead period. Constant
#' adjustments are only available for the model period. This method
#' also initialises all model timeseries with \code{NA} and all constant
#' adjusments with 0.}
#' }
MacroModel <- R6Class("MacroModel",
    cloneable = FALSE,
    public = list(
        maxlag = "integer",
        maxlead = "integer",
        model_period = "regperiod_range",
        model_data_period = "regperiod_range",

        initialize = function(mif_name) {

            private$model_index <- .Call(read_mdl_c, mif_name)

            # get maximum lag and lead
            ret <- .Fortran("get_max_lag_lead_fortran",
                            model_index = private$model_index, maxlag = 1L,
                            maxlead = 1L)
            self$maxlag <- ret$maxlag
            self$maxlead <- ret$maxlead

            reg.finalizer(self,
               function(e) {.Fortran("remove_mws_fortran",
                                     model_index = private$model_index)},
                onexit = TRUE)
        },
        get_variable_names = function() {
            return (sort(.Call(get_names_c, "data",
                               as.integer(private$model_index))))
        },
        get_ca_names = function() {
            # note: the names returned  by get_ca_names are not sorted
            # alphabetically, therefore sort explicitly
            return (sort(.Call(get_names_c, "ca",
                               as.integer(private$model_index))))
        },
        set_period = function(period) {
            period <- as.regperiod_range(period)
            self$model_period <- period
            p1 <- start_period(period)
            p2 <- end_period(period)
            start <- as.integer(c(get_year(p1), get_subperiod(p1)))
            end   <- as.integer(c(get_year(p2), get_subperiod(p2)))
            retval <- .Fortran("set_period_fortran",
                               model_index = private$model_index,
                               start = start, end = end,
                               freq  = as.integer(period[3]), ier = 1L)

            self$model_data_period <- regperiod_range(
                                start_period(self$model_period) - self$maxlag,
                                end_period(self$model_period) + self$maxlead)
            return (invisible(self))
        },
        get_data = function(names = self$get_variable_names(),
                            period = self$model_data_period) {
            "Returns the model data"
            if (!missing(names)) {
                names <- intersect(names, self$get_variable_names())
            }
            return (private$get_variables("data", names, period))
        },
        get_ca = function(names = self$get_ca_names(),
                          period = self$model_period) {
            "Returns the constant adjustments"
            if (!missing(names)) {
                names <- intersect(names, self$get_ca_names())
            }
            return (private$get_variables("ca", names, period))
        },
        get_fix = function() {
            "Returns the fix values"
            return (private$get_fix_fit(self, type = "fix"))
        },
        get_fit = function() {
            "Returns the fit targets"
            return (private$get_fix_fit(self, type = "fit"))
        },
        set_data = function(ts_data) {
            "Sets the model data"
            return (private$set_var(1L, ts_data, substitute(ts_data)))
        },
        set_ca = function(ts_data) {
            "Sets the constant adjustments"
            return (private$set_var(2L, ts_data, substitute(ts_data)))
        },
        set_fix = function(ts_data) {
            "Sets the fix data"
            return (private$set_var(3L, ts_data, substitute(ts_data)))
        },
        set_fit = function(ts_data) {
            "Sets the fit data"
            return (private$set_var(4L, ts_data, substitute(ts_data)))
        },
        set_rms = function(rms_list) {
            "Sets the rms values"
            return (invisible(.Call(set_rms_c, private$model_index, rms_list)))
        },
        set_solve_options = function(options = list()) {
            "Set  the default solve options"
            .Call("set_solve_opts_c", private$model_index, options)
            return (invisible(self))
        },
        solve = function(period = self$model_period, options = list()) {
            "Solve the model for the specified period"
            private$check_period_set()
            js <- private$get_period_indices(period)
            .Call("solve_c", model_index = private$model_index,
                             jtb = js$startp, jte = js$endp, options)
            return (invisible(self))
        },
        fill_mdl_data = function(period = self$model_data_period) {
            "Calculates missing model data from identities."
            private$check_period_set()
            js <- private$get_period_indices(period)
            .Call("filmdt_c", model_index = private$model_index,
                            jtb = js$startp, jte = js$end)
            return (invisible(self))
        },
        get_mws = function() {
            "Returns an mws object"
            private$check_period_set()

            data <- self$get_data()
            ca   <- self$get_ca()

            # remove columns /rows with only NA from data
            # todo: skip leading/trailing rows with only NA
            data <- data[ , ! apply(is.na(data) , 2 , all)]

            # remove columns with only 0 from ca
            # todo: skip leading/trailing columns with only NA
            ca <- ca[, !apply(ca == 0, 2, all)]

            # todo: rms values

            return (structure(list(var_names = self$get_variable_names(),
                                   ca_names = self$get_ca_names(),
                                   model_period = self$model_period,
                                   data = data, ca = ca,
                                   fix = self$get_fix(),
                                   fit = self$get_fit()),
                              class="mws"))
        },
        set_mws = function(x) {
            "Sets the model workspace"
            if (!inherits(x, "mws")) {
                stop("x is not an mws object")
            }
            if (!identical(x$var_names, self$get_variable_names()) |
                !identical(x$ca_names, self$get_ca_names())) {
                    stop("Mws x does not agree with the model definition")
            }
            self$set_period(x$model_period)
            self$set_data(x$data)
            self$set_ca(x$ca)
            if (!is.null(x$fix)) {
                self$set_fix(x$fix)
            }
            if (!is.null(x$fit)) {
                self$set_fit(x$fit)
            }
        },
        get_solve_options = function() {
            "Gets the default solve options"
            return (.Call("get_solve_opts_c", 
                          model_index = private$model_index))
        }
    ),
    private = list(
        model_index = "integer",
        get_period_indices = function(period, extended = TRUE) {
            period <- as.regperiod_range(period)
            mdl_period_start <- start_period(self$model_period)
            startp <- as.integer(start_period(period) - mdl_period_start + 1)
            endp   <- as.integer(end_period(period)   - mdl_period_start + 1)
            return (list(startp = startp, endp = endp))
        },
        check_period_set = function() {
            # Check if the model_period has been set
            if (is.null(self$model_period)) {
                stop(paste("The model period is not set.",
                            "Set the model period with set_period()"))
            }
            return (NULL)
        },
        set_var = function(set_type, ts_data, ts_data_expr) {
            # general function used to update model data, constant adjustments,
            # fix values or fit targets.
            ts_names <- colnames(ts_data)
            if (is.null(ts_names)) {
                if (!is.mts(ts_data)) {
                    ts_names <- deparse(ts_data_expr)
                } else {
                    stop(paste0("in ", deparse(sys.call(-1)),
                               "\n  Argument ts_data does not have colnames"),
                         call. = FALSE)
                }
            }
            private$check_period_set()
            ts_data <- as.regts(ts_data)
            shift <- private$get_period_indices(
                                 get_regperiod_range(ts_data))$startp
            if (!is.matrix(ts_data)) {
                dim(ts_data) <- c(length(ts_data), 1)
            }
            if (is.integer(ts_data)) {
                # make sure that data is a matrix of real values and no integers
                ts_data[] <- as.numeric(ts_data)
            }
            .Call(set_c, set_type, private$model_index, ts_data, ts_names,
                  shift)

            # todo: report number of timeseries that have not been set
            return (invisible(self))
        },
        get_variables = function(type, names, period) {
            # general function used to get model data or constant adjustments
            private$check_period_set()
            period <- as.regperiod_range(period)
            if (length(names) > 0) {
                js <- private$get_period_indices(period)
                data <- .Call("get_data_c", type = type,
                              model_index = private$model_index,
                              names = names, jtb = js$startp, jte = js$endp)
                return (regts(data, start = start_period(period), names = names))
            } else {
                return (NULL)
            }
        },
        get_fix_fit = function(mdl, type) {
            # general function for getting fix or fit values
            ret <- .Call("get_fix_fit_c", type = type,
                         model_index = private$model_index)
            if (!is.null(ret)) {
                ret <- regts(ret[[2]], start = start_period(self$model_period)
                             + ret[[1]] - 1, names = ret[[3]])
                ret <- ret[, sort(colnames(ret))]
            }
            return (ret)
        }
    )
)


