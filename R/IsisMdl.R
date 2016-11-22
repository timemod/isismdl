library(R6)

#' @importFrom methods setOldClass
setOldClass("regperiod_range")

#' An R6 class class representing a macroecomic model
#'
#' @docType class
#' @importFrom R6 R6Class
#' @useDynLib isismdl read_mdl_c
#' @useDynLib isismdl get_max_lag_lead_fortran
#' @useDynLib isismdl get_var_names_c
#' @useDynLib isismdl get_eq_names_c
#' @useDynLib isismdl get_par_names_c
#' @useDynLib isismdl set_period_fortran
#' @useDynLib isismdl get_param_c
#' @useDynLib isismdl get_data_c
#' @useDynLib isismdl get_fix_fit_c
#' @useDynLib isismdl set_param_c
#' @useDynLib isismdl set_c
#' @useDynLib isismdl set_rms_c
#' @useDynLib isismdl set_solve_opts_c
#' @useDynLib isismdl get_solve_opts_c
#' @useDynLib isismdl set_fit_opts_c
#' @useDynLib isismdl solve_c
#' @useDynLib isismdl filmdt_c
#' @useDynLib isismdl remove_mws_fortran
#' @useDynLib isismdl set_cvgcrit_c
#' @useDynLib isismdl set_cvgcrit_set_mws
#' @useDynLib isismdl get_cvgcrit_c
#' @useDynLib isismdl set_ftrelax_c
#' @useDynLib isismdl set_ftrelax_set_mws
#' @useDynLib isismdl get_ftrelax_c
#' @useDynLib isismdl set_eq_status_c
#' @useDynLib isismdl mdlpas_fortran
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
#' # create the ISLM model based on the mif file
#' islm_model <- read_mdl("islm.mif")
#'
#' #  set modelworkspace using example mws islm_input_mws
#' islm_model$set_mws(islm_input_mws)
#' islm_model$solve()
#' \dontshow{
#' unlink("islm.*")
#' }
#'
#' @section Methods:
#' \describe{
#'
#' \item{\code{get_var_names(pattern = ".*", vtype = c("all", "allfrml",
#' "all_endolead"))}}{Returns the names of the model variables. \code{pattern} is
#' a regular expression, \code{vtype} the variable type.}
#'
#' \item{\code{get_par_names(pattern = ".*")}}{Returns
#' the names of the model parameters \code{pattern} is a regular expression.}
#'
#' \item{\code{get_eq_names(pattern = ".*", vtype = c("all", "inactive"))}}{Returns
#' the names of the equations. \code{pattern} is a regular expression}
#'
#' \item{\code{set_period(period)}}{Sets the model period. \code{period}
#' is a \code{\link{regperiod_range}} object or an object that can be coerced
#' to a \code{regperiod_range}. The model period is the longest period for which
#' the model may be solved. This method also allocates storage for
#' all model timeseries and constant adjustments. Model timeseries are
#' available for the so called 'model data period', which is
#' the model period extended with a lag and lead period. Constant
#' adjustments are only availabcdle for the model period. This method
#' also initialises all model timeseries with \code{NA} and all constant
#' adjusments with 0.}
#'
#' \item{\code{get_period()}}{Returns the model period}
#'
#' \item{\code{get_data_period()}}{Returns the model data period}
#'
#' \item{\code{set_param(p)}}{Sets the model parameter. \code{p} is a named
#' list.}
#'
#' \item{\code{get_param(pattern, names)}}{Returns the model parameter.
#' \code{pattern} is a regular expression, \code{names} is a vector with parameter
#' names.}
#' }
IsisMdl <- R6Class("IsisMdl",
    cloneable = FALSE,
    public = list(

        initialize = function(mif_name) {

            private$model_index <- .Call(read_mdl_c, mif_name)

            # get maximum lag and lead
            ret <- .Fortran("get_max_lag_lead_fortran",
                            model_index = private$model_index, maxlag = 1L,
                            maxlead = 1L)
            private$maxlag <- ret$maxlag
            private$maxlead <- ret$maxlead

            reg.finalizer(self,
               function(e) {.Fortran("remove_mws_fortran",
                                     model_index = private$model_index)},
                onexit = TRUE)
        },
        get_var_names = function(pattern = ".*", vtype = "all") {
            names <- .Call(get_var_names_c, vtype, private$model_index)
            if (!missing(pattern)) {
                sel <- grep(pattern, names)
                names <- names[sel]
            }
            if (vtype == "allfrml" || vtype == "all_endolead") {
                names <- sort(names)
            }
            return (names)
        },
        get_par_names = function(pattern = ".*") {
            names <- .Call(get_par_names_c, private$model_index)
            if (!missing(pattern)) {
                sel <- grep(pattern, names)
                names <- names[sel]
            }
            return (names)
        },
        get_eq_names = function(pattern = ".*", type = "all") {
            names <- .Call("get_eq_names_c", type, private$model_index)
            if (!missing(pattern)) {
                sel <- grep(pattern, names)
                names <- names[sel]
            }
            return (names)
        },
        set_period = function(period) {
            period <- as.regperiod_range(period)
            private$model_period <- period
            p1 <- start_period(period)
            p2 <- end_period(period)
            start <- as.integer(c(get_year(p1), get_subperiod(p1)))
            end   <- as.integer(c(get_year(p2), get_subperiod(p2)))
            retval <- .Fortran("set_period_fortran",
                               model_index = private$model_index,
                               start = start, end = end,
                               freq  = as.integer(period[3]), ier = 1L)

            private$model_data_period <- regperiod_range(
                                start_period(private$model_period) - private$maxlag,
                                end_period(private$model_period) + private$maxlead)
            return (invisible(self))
        },
        get_period = function() {
            return (private$model_period)
        },
        get_data_period = function() {
            return (private$model_data_period)
        },
        set_param = function(p) {
            "Sets the model parameters"
            if (!is.list(p)) {
                stop("Argument p is not a list")
            }
            if (is.null(names(p))) {
                stop("Argument p has no names")
            }

            # check if the list contains any non-numeric elements
            is_num <- unlist(lapply(p, FUN = function(x) !is.numeric(x)))
            if (any(is_num)) {
                no_numeric <- names(p)[is_num]
                stop(concat_names(no_numeric), " not numeric")
            }

            # convert integer list elements to numeric
            p <- lapply(p, as.numeric)
            nset <- .Call("set_param_c", model_index = private$model_index, p)
            if (nset < length(p)) {
                no_params <- setdiff(names(p), self$get_par_names())
                warning(concat_names(no_params), " no model parameter(s)")
            }
            return (invisible(self))
        },
        get_param = function(pattern, names) {
            # Return the parameters vaLues
            if (missing(pattern) && missing(names)) {
                names <- self$get_par_names()
            } else if (missing(names)) {
                names <- self$get_par_names(pattern)
            } else if (!missing(pattern)) {
                names <- union(names, self$get_par_names(pattern))
            }
            return (.Call("get_param_c", model_index = private$model_index,
                          names = names))
        },
        get_data = function(pattern, names, period = private$model_data_period) {
            "Returns the model data"
            if (is.null(private$model_period)) stop(private$period_error_msg)
            period <- as.regperiod_range(period)
            if (missing(pattern) && missing(names)) {
                names <- self$get_var_names()
            } else if (missing(names)) {
                names <- self$get_var_names(pattern)
            } else if (!missing(pattern)) {
                names <- union(names, self$get_var_names(pattern))
            }
            js <- private$get_period_indices(period)
            data <- .Call("get_data_c", type = "data",
                           model_index = private$model_index,
                           names = names, jtb = js$startp, jte = js$endp)
            return (regts(data, start = start_period(period), names = names))
        },
        get_ca = function(pattern, names, period = private$model_period) {
            "Returns the constant adjustments"
            if (is.null(private$model_period)) stop(private$period_error_msg)
            period <- as.regperiod_range(period)
            if (missing(pattern) && missing(names)) {
                names <- self$get_var_names(vtype = "allfrml")
            } else if (missing(names)) {
                names <- self$get_var_names(pattern, vtype = "allfrml")
            } else if (!missing(pattern)) {
                names <- union(names,
                               self$get_var_names(pattern, vtype = "allfrml"))
            }
            js <- private$get_period_indices(period)
            data <- .Call("get_data_c", type = "ca",
                           model_index = private$model_index,
                           names = names, jtb = js$startp, jte = js$endp)
            return (regts(data, start = start_period(period), names = names))
        },
        get_fix = function() {
            "Returns the fix values"
            return (private$get_fix_fit(type = "fix"))
        },
        get_fit = function() {
            "Returns the fit targets"
            return (private$get_fix_fit(type = "fit"))
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
        set_solve_options = function(...) {
            "Set  the default solve options"
            .Call("set_solve_opts_c", private$model_index, list(...))
            return (invisible(self))
        },
        set_fit_options = function(...) {
            "Set  the default solve options"
            .Call("set_fit_opts_c", private$model_index, list(...))
            return (invisible(self))
        },
        solve = function(period = private$model_period, options = list(),
                         fit_options = list()) {
            "Solve the model for the specified period"
            if (is.null(private$model_period)) stop(private$period_error_msg)
            js <- private$get_period_indices(period)
            .Call("solve_c", model_index = private$model_index,
                             jtb = js$startp, jte = js$endp, options,
                             fit_options)
            return (invisible(self))
        },
        fill_mdl_data = function(period = private$model_data_period) {
            "Calculates missing model data from identities."
            if (is.null(private$model_period)) stop(private$period_error_msg)
            js <- private$get_period_indices(period)
            .Call("filmdt_c", model_index = private$model_index,
                            jtb = js$startp, jte = js$end)
            return (invisible(self))
        },
        get_mws = function() {
            "Returns an mws object"
            if (is.null(private$model_period)) stop(private$period_error_msg)

            data <- self$get_data()
            ca   <- self$get_ca()

            # remove columns /rows with only NA from data
            # todo: skip leading/trailing rows with only NA
            data <- data[ , ! apply(is.na(data) , 2 , all), drop = FALSE]

            # remove columns with only 0 from ca
            # todo: skip leading/trailing columns with only 0
            ca <- ca[, !apply(ca == 0, 2, all), drop = FALSE]

            # todo: rms values

            return (structure(list(var_names = self$get_var_names(),
                                   ca_names = self$get_var_names(vtype = "allfrml"),
                                   model_period = private$model_period,
                                   solve_options = self$get_solve_options(),
                                   cvgcrit = .Call("get_cvgcrit_c",
                                                   private$model_index, 0L),
                                   ftrelax = .Call("get_ftrelax_c", private$model_index),
                                   param = self$get_param(),
                                   data = data, ca = ca,
                                   fix = self$get_fix(),
                                   fit = self$get_fit(),
                                   inactive_eqs = self$get_eq_names(type = "inactive")),
                              class="mws"))
        },
        set_mws = function(x) {
            "Sets the model workspace"
            if (!inherits(x, "mws")) {
                stop("x is not an mws object")
            }
            if (!identical(x$var_names, self$get_var_names()) |
                !identical(x$ca_names, self$get_var_names(vtype = "allfrml")) |
                !identical(names(x$param), self$get_par_names())) {
                     # todo: check parameter length, this should be the same
                    stop("Mws x does not agree with the model definition")
            }
            #todo: check endogenous leads etc.
            self$set_period(x$model_period)
            do.call(self$set_solve_options, x$solve_options)
            .Call("set_cvgcrit_set_mws", private$model_index, x$cvgcrit)
            .Call("set_ftrelax_set_mws", private$model_index, x$ftrelax)
            self$set_param(x$param)
            self$set_data(x$data)
            self$set_ca(x$ca)
            if (!is.null(x$fix)) {
                self$set_fix(x$fix)
            }
            if (!is.null(x$fit)) {
                self$set_fit(x$fit)
            }

            # make all equations active execpt for the inactive equations
            self$set_eq_status("active")
            if (length(x$inactive_eqs) > 0) {
                self$set_eq_status("inactive", vars = x$inactive_eqs)
            }
            return (invisible(self))
        },
        get_solve_options = function() {
            "Gets the default solve options"
            return (.Call("get_solve_opts_c",
                          model_index = private$model_index))
        },
        get_cvgcrit = function() {
            values <- .Call("get_cvgcrit_c", private$model_index, 1L)
            names(values) <- self$get_var_names()
            return (values)
        },
        set_cvgcrit = function(value, pattern, names) {
            "Sets the convergence criterion for some variables"
            if (missing(pattern) && missing(names)) {
                names <- self$get_var_names()
            } else if (!missing(pattern)) {
                pvars <- self$get_var_names(pattern)
                if (!missing(names)) {
                    names <- union(pvars, names)
                } else {
                    names <- pvars
                }
            }
            if (!is.numeric(value) || length(value) != 1) {
                stop("value should be a single numerical value")
            }
            .Call("set_cvgcrit_c", private$model_index, names, as.numeric(value))
            return  (invisible(self))
        },
        set_ftrelax = function(value, pattern, names) {
            "Sets the Fair-Taylor relaxation criterion."
            if (missing(pattern) && missing(names)) {
                names <- self$get_var_names(vtype = "all_endolead")
            } else if (!missing(pattern)) {
                pvars <- self$get_var_names(pattern, vtype = "all_endolead")
                if (!missing(names)) {
                    names <- union(pvars, names)
                } else {
                    names <- pvars
                }
            }
            if (!is.numeric(value) || length(value) != 1) {
                stop("value should be a single numerical value")
            }
            .Call("set_ftrelax_c", private$model_index, names, as.numeric(value))
            return  (invisible(self))
        },
        get_ftrelax = function() {
            "Returns the Fair-Taylor relaxtion factors"
            values <- .Call("get_ftrelax_c", private$model_index)
            names(values) <- .Call(get_var_names_c, "all_endolead",
                                   private$model_index)
            sorted_names <- sort(names(values))
            return (values[sorted_names])
        },
        set_eq_status = function(stat, pattern, names) {
            "Activate or deactivate equations"

            # TODO: if vars specified, then check if it contains names that are
            # no model variables

            if (missing(pattern) && missing(names)) {
                names <- self$get_eq_names()
            } else if (!missing(pattern)) {
                pvars <- self$get_eq_names(pattern)
                if (!missing(names)) {
                    names <- union(pvars, names)
                } else {
                    names <- pvars
                }
            }
            if (!is.character(stat) || length(stat) != 1) {
                stop("value should be a single character value")
            }
            .Call("set_eq_status_c", private$model_index, names, stat);
            return  (invisible(self))
        },
        mdlpas = function(period = private$model_period) {
            "Run all equations of the model in solution order forwards in time"
            if (is.null(private$model_period)) stop(private$period_error_msg)
            js <- private$get_period_indices(period)
            ret <- .Fortran("mdlpas_fortran",
                            model_index = private$model_index,
                            jtb = js$startp, jte = js$end)
            return (invisible(self))
        }
    ),
    private = list(
        maxlag = NA_integer_,
        maxlead = NA_integer_,
        model_period = NULL,
        model_data_period = NULL,
        model_index = NA_integer_,
        period_error_msg = paste("The model period is not set.",
                            "Set the model period with set_period()."),
        get_period_indices = function(period, extended = TRUE) {
            period <- as.regperiod_range(period)
            mdl_period_start <- start_period(private$model_period)
            startp <- as.integer(start_period(period) - mdl_period_start + 1)
            endp   <- as.integer(end_period(period)   - mdl_period_start + 1)
            return (list(startp = startp, endp = endp))
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
            if (is.null(private$model_period)) stop(private$period_error_msg)
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
            if (is.null(private$model_period)) stop(private$period_error_msg)
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
        get_fix_fit = function(type) {
            # general function for getting fix or fit values
            ret <- .Call("get_fix_fit_c", type = type,
                         model_index = private$model_index)
            if (!is.null(ret)) {
                ret <- regts(ret[[2]], start = start_period(private$model_period)
                             + ret[[1]] - 1, names = ret[[3]])
                ret <- ret[, sort(colnames(ret))]
            }
            return (ret)
        }
    )
)

# utility function for error / warning messages: concate a number of names,
# separating the first n - 1 names with "," and the last with "and".
# Finally, "is" or "are" are added depending on the number of names.
concat_names <- function(names) {
    n <- length(names)
    if (n == 1) {
        return (paste(names, "is"))
    } else {
        return (paste(paste(names[-n], collapse = ", "), "and", names[n], "are"))
    }
}


