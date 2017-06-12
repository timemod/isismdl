#' @importFrom methods setOldClass
setOldClass("period_range")

#' An R6 class class representing an Isis model
#'
#' This class is used to solve a system of non-linear equations with lagged
#' variables. The model equations are specified in a separate text file, the so
#' called model file. Function  \code{\link{isis_mdl}} parses the model file
#' and generates an \code{IsisMdl} object. The vignette "Introduction"
#' gives a detailed description of the usage of \code{IsisMdl} objects.
#'
#' The syntax of the model file is the same of the syntax of model file
#' in Isis, and is described in detail in the Isis Reference Manual
#' (a vignette with a detailed model syntax description for package IsisMdl will be
#' available in a future).
#'
#' The package included a number of example models in directory \code{models} of
#' the package library. Function \code{\link{copy_example_mdl}} is a convenience
#' function that can be used to copy these example models to your working directory.
#' It is also possible to directly create \code{IsisMdl} objects with functions
#' \code{\link{islm_mdl}} to create an ISLM model and \code{\link{ifn_mdl}}
#' to create another example model, the IFN model. The latter model is a model with
#' leads and can be solved with the Fair-Taylor-method.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @useDynLib isismdl read_mdl_c
#' @useDynLib isismdl write_mdl_c
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
#' @useDynLib isismdl clone_mws_fortran
#' @useDynLib isismdl run_eqn_fortran
#' @import regts
#' @importFrom "methods" "new"
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} representing an Isis model.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # create an example ISLM  model
#' mdl <- islm_mdl()
#'
#' # prepare input timeseries
#' r  <- regts(3.35, start = "2015Q1", end = "2016Q3", labels = "interest rate")
#' y  <- regts(980,  start = "2015Q1", end = "2016Q3", labels = "income")
#' yd <- regts(790, start = "2015Q1", labels = "disposable income")
#' g  <- regts(210 * cumprod(rep(1.015, 6)), start = "2015Q2",
#'             labels = "government spending")
#' ms <- regts(200 * cumprod(rep(1.015, 6)), start = "2015Q2",
#'            labels = "money supply")
#' islm_input <- cbind(r, y, yd, g, ms)
#' print(islm_input)
#'
#' # set period and update model timeseries
#' mdl$set_period("2015Q2/2016Q3")
#' mdl$set_data(islm_input)
#'
#' mdl$set_labels(c(i = "investment", c = "consumption", md = "money demand",
#'                 t = "tax"))
#'
#' mdl$solve()
#'
#' \dontshow{
#' unlink("islm.*")
#' }
#'
#' @section Methods:
#'
#' \code{IsisMdl} objects support the following methods. These methods
#' are described in detail in the different subsection of the documentation
#' For example, method \code{solve} is described in section
#' \code{\link{solve}}.
#
#' \describe{
#'
#' \item{\code{\link{get_maxlag}}}{Returns the maximum lag}
#'
#' \item{\code{\link{get_maxlead}}}{Returns the maximum lead}
#'
#' \item{\code{\link{get_var_names}}}{Returns the names of the model
#' variables}
#'
#' \item{\code{\link{get_par_names}}}{Returns the names of the model parameters}
#'
#' \item{\code{\link{get_eq_names}}}{Returns the names of the equations}
#'
#' \item{\code{\link{init_data}}}{Initializes the model data}
#'
#' \item{\code{\link{set_period}}}{Sets the model period}
#'
#' \item{\code{\link{get_period}}}{Returns the model period}
#'
#' \item{\code{\link{get_data_period}}}{Returns the model data period}
#'
#' \item{\code{\link{set_labels}}}{Set labels for the model variables}
#'
#' \item{\code{\link{get_labels}}}{Returns the labels of the model variables.}
#'
#' \item{\code{\link{set_param}}}{Sets the model parameter}
#'
#' \item{\code{\link{get_param}}}{Returns model parameters}
#'
#' \item{\code{\link{set_data}}}{Transfer timeseries to the model data}
#'
#' \item{\code{\link{set_ca}}}{Transfer timeseries to the constant adjustments}
#'
#' \item{\code{\link{set_fix}}}{Transfer timeseries to the fix values}
#'
#' \item{\code{\link{set_fit}}}{Transfer timeseries to the fit targets}
#'
#' \item{\code{\link{set_values}}}{Sets the values of the model data}
#'
#' \item{\code{\link{set_ca_values}}}{Sets the values of the constant
#' adjustments}
#'
#' \item{\code{\link{set_fix_values}}}{Sets the fix values}
#'
#' \item{\code{\link{set_fit_values}}}{Sets the values of the fit targets}
#'
#' \item{\code{\link{set_fit}}}{Transfer timeseries to the fit targets}
#'
#' \item{\code{\link{change_data}}}{Change model data by applying a function}
#'
#' \item{\code{\link{change_ca}}}{Change the constant adjusmtments by
#' applying a function}
#'
#' \item{\code{\link{get_data}}}{Returns the model data}
#'
#' \item{\code{\link{get_ca}}}{Returns the constant adjustments}
#'
#' \item{\code{\link{get_fix}}}{Returns the fix values}
#'
#' \item{\code{\link{get_fit}}}{Returns the fit targets}
#'
#' \item{\code{\link{set_rms}}}{Sets or updates  the rms values}
#'
#' \item{\code{\link{set_solve_options}}}{Sets the solve options}
#'
#' \item{\code{\link{set_cvgcrit}}}{Sets the convergence criterion for selected
#' variables}
#'
#' \item{\code{\link{set_cvgcrit}}}{Returns the convergence criterion for selected
#' variables}
#'
#' \item{\code{\link{set_eq_status}}}{Sets the equation status 
#' \code{"active"} or \code{"inactive")}}
#'
#' \item{\code{\link{get_eq_status}}}{}{Returns the equation status 
#' \code{"active"} or \code{"inactive")}}
#'
#' \item{\code{\link{set_ftrelax}}}{Sets the Fair-Taylor relaxtion factors}
#'
#' \item{\code{\link{get_ftrelax}}}{Returns the Fair-Taylor relaxtion factors}
#'
#' \item{\code{\link{solve}}}{Solves the model}
#'
#' \item{\code{\link{fill_mdl_data}}}{Calculates missing model
#' data from identities}
#'
#' \item{\code{\link{write_mdl}}}{Serializes the model object and writes it
#' to an RDS file}
#' }
#' @seealso \code{\link{isis_mdl}}, \code{\link{islm_mdl}}
#' and \code{\link{ifn_mdl}}
#'
IsisMdl <- R6Class("IsisMdl",
  public = list(
    initialize = function(mif_name, mws = NULL) {

      cat(paste("Reading mif file", mif_name, "...\n"))
      private$model_index <- .Call(read_mdl_c, mif_name)

      # get maximum lag and lead
      ret <- .Fortran("get_max_lag_lead_fortran",
                      model_index = private$model_index, maxlag = 1L,
                      maxlead = 1L)
      private$maxlag <- ret$maxlag
      private$maxlead <- ret$maxlead
      private$var_names <- .Call(get_var_names_c, "all",
                                 private$model_index)
      private$var_count <- length(private$var_names)
      private$labels    <- character(0)
      names(private$labels) <- character(0)
      reg.finalizer(self,
                    function(e) {.Fortran("remove_mws_fortran",
                                          model_index = private$model_index)},
                    onexit = TRUE)
      if (!missing(mws)) {
        private$set_mws(mws)
      }
    },
    print = function(...) {
      cat("IsisModel object\n")
      cat(sprintf("%-60s%d\n", "Model index:", private$model_index))
      cat(sprintf("%-60s%d\n", "Number of variables:", private$var_count))
      cat(sprintf("%-60s%d\n", "Maximum lag:", private$maxlag))
      cat(sprintf("%-60s%d\n", "Maximum lead:", private$maxlead))
      if (!is.null(private$model_period)) {
        cat(sprintf("%-60s%s\n", "Model period:",
                    as.character(private$model_period)))
        cat(sprintf("%-60s%s\n", "Model data period:",
                    as.character(private$data_period)))
      }
    },
    get_maxlag = function() {
      return(private$maxlag)
    },
    get_maxlead = function() {
      return(private$maxlead)
    },
    get_var_names = function(pattern = ".*", 
                             type = c("all", "allfrml", "all_endolead")) {
      type <- match.arg(type)
      if  (type != "all") {
        names <- .Call(get_var_names_c, type, private$model_index)
      } else {
        names <- private$var_names
      }
      if (!missing(pattern)) {
        sel <- grep(pattern, names)
        names <- names[sel]
      }
      if (type == "allfrml" || type == "all_endolead") {
        names <- sort(names)
      }
      return(names)
    },
    set_labels = function(labels) {
      private$update_labels(labels)
    },
    get_labels = function() {
      return(private$labels)
    },
    get_par_names = function(pattern = ".*") {
      names <- .Call(get_par_names_c, private$model_index)
      if (!missing(pattern)) {
        sel <- grep(pattern, names)
        names <- names[sel]
      }
      return (names)
    },
    get_eq_names = function(pattern = ".*", type = "all",
                            order = c("sorted", "solve", "natural")) {
      order <- match.arg(order)
      names <- .Call("get_eq_names_c", type, private$model_index, order)
      if (!missing(pattern)) {
        sel <- grep(pattern, names)
        names <- names[sel]
      }
      return (names)
    },
    init_data = function(data_period, data, ca) {

      if (missing(data_period)) {
        if (!missing(data)) {
          data_period <- get_period_range(data)
        } else {
          stop(paste("Argument data_period is mandatory if",
                     "argument data has not been specified"))
        }
      }
      private$init_data_(data_period)

      # update the model period
      private$model_period <- period_range(
        start_period(data_period) + private$maxlag,
        end_period(data_period)   - private$maxlead)

      if (!missing(data)) {
        self$set_data(data)
      }
      if (!missing(ca)) {
        self$set_ca(ca)
      }
    },
    set_period = function(period) {
      period <- as.period_range(period)

      if (is.null(private$data_period)) {
        data_period <- period_range(
          start_period(period) - private$maxlag,
          end_period(period)   + private$maxlead)
        private$init_data_(data_period)
      } else  {
        private$check_model_period(period)
      }
      private$model_period <- period
      return (invisible(self))
    },
    get_period = function() {
      return (private$model_period)
    },
    get_data_period = function() {
      return (private$data_period)
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
    set_data = function(data, names = colnames(data)) {
      "Sets the model data"
      if (is.null(private$data_period)) {
        stop(paste("The data period has not been set yet.",
                   "Please initialize the with model data with init_data().",
                   "E.g.: init_data(data)"))
      }
      return(private$set_var(1L, data, names, missing(names)))
    },
    set_ca = function(data, names = colnames(data)) {
      "Sets the constant adjustments"
      return (private$set_var(2L, data, names, missing(names)))
    },
    set_fix = function(data, names = colnames(data)) {
      "Sets the fix data"
      return (private$set_var(3L, data, names, missing(names)))
    },
    set_fit = function(data, names = colnames(data)) {
      "Sets the fit data"
      return (private$set_var(4L, data, names, missing(names)))
    },
    get_data = function(pattern, names,
                        period = private$data_period) {
      "Returns the model data"
      if (is.null(private$model_period)) stop(private$period_error_msg)
      period <- as.period_range(period)
      if (missing(pattern) && missing(names)) {
        names <- private$var_names
      } else if (missing(names)) {
        names <- self$get_var_names(pattern)
      } else if (!missing(pattern)) {
        names <- union(names, self$get_var_names(pattern))
      }
      js <- private$get_period_indices(period)
      data <- .Call("get_data_c", type = "data",
                    model_index = private$model_index,
                    names = names, jtb = js$startp, jte = js$endp)
      ret <- regts(data, start = start_period(period), names = names)
      if (length(private$labels) > 0) {
        ret <- update_ts_labels(ret, private$labels)
      }
      return(ret)
    },
    get_ca = function(pattern, names, period = private$data_period) {
      "Returns the constant adjustments"
      if (is.null(private$model_period)) stop(private$period_error_msg)
      period <- as.period_range(period)
      if (missing(pattern) && missing(names)) {
        names <- self$get_var_names(type = "allfrml")
      } else if (missing(names)) {
        names <- self$get_var_names(pattern, type = "allfrml")
      } else if (!missing(pattern)) {
        names <- union(names,
                       self$get_var_names(pattern, type = "allfrml"))
      }
      if (length(names) == 0) {
        return(NULL)
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
    set_values = function(value, names = NULL, pattern = NULL,
                          period = private$data_period) {
      return (private$set_values_(1L, value, names, pattern, period))
    },
    set_ca_values = function(value, names = NULL, pattern = NULL,
                             period = private$data_period) {
      return (private$set_values_(2L, value, names, pattern, period))
    },
    set_fix_values = function(value, names = NULL, pattern = NULL,
                              period = private$data_period) {
      return (private$set_values_(3L, value, names, pattern, period))
    },
    set_fit_values = function(value, names = NULL, pattern = NULL,
                              period = private$data_period) {
      return (private$set_values_(4L, value, names, pattern, period))
    },
    change_data = function(fun, names = NULL, pattern = NULL,
                           period = private$data_period, ...) {
      return(private$change_var_(1L, fun, names, pattern, period, ...))
    },
    change_ca = function(fun, names = NULL, pattern = NULL,
                         period = private$data_period, ...) {
      return(private$change_var_(2L, fun, names, pattern, period, ...))
    },
    set_rms = function(values) {
      "Sets the rms values"
      # TODO: error if values is not numeric or integer
      names <- names(values)
      values <- as.numeric(values)
      # as.numeric does not preserve the names
      names(values) <- names
      .Call(set_rms_c, private$model_index, values)
      return (invisible(self))
    },
    set_solve_options = function(mode, fbstart, maxiter, maxjacupd, rlxspeed,
                                 rlxmin, rlxmax, cstpbk, cnmtrx, xrelax,
                                 xmaxiter, xupdate, dbgopt, erropt,
                                 report, ratreport, ratreport_rep,
                                 ratfullreport_rep, bktmax, xtfac) {

      # create a list of supplied options
      names <- names(match.call()[-1])
      options <- lapply(names, FUN = function(x) {eval(parse(text = x))})
      names(options) <- names
      .Call("set_solve_opts_c", private$model_index, options)
      return (invisible(self))
    },
    set_fit_options = function(maxiter, cvgabs, mkdcrt, report, dbgopt) {
      names <- names(match.call()[-1])
      options <- lapply(names, FUN = function(x) {eval(parse(text = x))})
      .Call("set_fit_opts_c", private$model_index, options)
      return (invisible(self))
    },
    solve = function(period = private$model_period, options = list(),
                     fit_options = list()) {
      "Solve the model for the specified period"
      if (is.null(private$model_period)) stop(private$period_error_msg)
      private$check_options(options, type = "solve_options")
      private$check_options(options, type = "fit_options")
      period <- as.period_range(period)
      private$check_model_period(period)
      js <- private$get_period_indices(period)
      .Call("solve_c", model_index = private$model_index,
            jtb = js$startp, jte = js$endp, options,
            fit_options)
      return (invisible(self))
    },
    fill_mdl_data = function(period = private$data_period,
                             report = c("period", "minimal", "no")) {
      # Calculates missing model data from identities.
      report <- match.arg(report)
      if (is.null(private$model_period)) stop(private$period_error_msg)
      period <- as.period_range(period)
      js <- private$get_period_indices(period)
      .Call("filmdt_c", model_index = private$model_index,
            jtb = js$startp, jte = js$end,
            report = report)
      return (invisible(self))
    },
    run_eqn = function(pattern, names, period = private$data_period) {
      "Run model equations"
      period <- as.period_range(period)
      if (missing(pattern) && missing(names)) {
        eq_names <- self$get_eq_names(order = "solve")
      } else if (missing(pattern) && !missing(names)) {
        eq_names <- intersect(names, self$get_eq_names())
        eq_nums <- as.integer(match(eq_names, self$get_eq_names()))
      } else if (!missing(pattern) && missing(names)) {
        eq_names <- self$get_eq_names(order = "solve")
        ieqs <- grep(pattern, eq_names)
        eq_names <- eq_names[ieqs]
      } else {
        stop(paste("Only one of arguments pattern and names can  be",
                   "specified"))
      }
      eqnums <- as.integer(match(eq_names, self$get_eq_names()))
      neq <- as.integer(length(eqnums))
      if (is.null(private$model_period)) stop(private$period_error_msg)
      js <- private$get_period_indices(period)
      .Fortran("run_eqn_fortran", model_index = private$model_index,
               neq = neq, eqnums = eqnums, jtb = js$startp, jte = js$end)
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
      .Call("set_cvgcrit_c", private$model_index, names,
            as.numeric(value))
      return  (invisible(self))
    },
    set_ftrelax = function(value, pattern, names) {
      "Sets the Fair-Taylor relaxation criterion."
      if (missing(pattern) && missing(names)) {
        names <- self$get_var_names(type = "all_endolead")
      } else if (!missing(pattern)) {
        pvars <- self$get_var_names(pattern, type = "all_endolead")
        if (!missing(names)) {
          names <- union(pvars, names)
        } else {
          names <- pvars
        }
      }
      if (!is.numeric(value) || length(value) != 1) {
        stop("value should be a single numerical value")
      }
      .Call("set_ftrelax_c", private$model_index, names,
            as.numeric(value))
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
    set_eq_status = function(status = c("active", "inactive"), 
                             pattern, names) {
      "Activate or deactivate equations"

      status <- match.arg(status)

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

      # TODO: if names specified, then check if it contains
      # names that are no model variables
      .Call("set_eq_status_c", private$model_index, names, status);
      return(invisible(self))
    },
    mdlpas = function(period = private$model_period) {
      "Run all equations of the model in solution order forwards in time"
      if (is.null(private$model_period)) stop(private$period_error_msg)
      js <- private$get_period_indices(period)
      ret <- .Fortran("mdlpas_fortran",
                      model_index = private$model_index,
                      jtb = js$startp, jte = js$end)
      return (invisible(self))
    },
    write_mdl = function(file) {
      # TODO: use tempfile
      mif_file <- "write_mdl.mif"
      .Call("write_mdl_c", mif_file, private$model_index)
      size <- file.info(mif_file)$size
      mif_data <- readBin(mif_file, what = "raw", n = size)
      serialized_mdl <- structure(list(mif_data = mif_data,
                                       mws = private$get_mws()),
                                  class = "serialized_isismdl")
      saveRDS(serialized_mdl, file)
      unlink(mif_file)
      return (invisible(self))
    }
  ),
  private = list(
    maxlag = NA_integer_,
    maxlead = NA_integer_,
    model_period = NULL,
    data_period = NULL,
    fortran_period = NULL,
    model_index = NA_integer_,
    var_names = NULL,
    var_count = NA_integer_,
    labels = NULL,
    period_error_msg = paste("The model period is not set.",
                             "Set the model period with set_period()."),
    deep_clone = function(name, value) {
      if (name == "model_index") {
        retval <- .Fortran("clone_mws_fortran", model_index = value,
                           model_index_clone = 1L)
        return (retval$model_index_clone)
      } else {
        return (value)
      }
    },
    get_period_indices = function(period, extended = TRUE) {
      period <- as.period_range(period)
      mdl_period_start <- start_period(private$fortran_period)
      startp <- as.integer(start_period(period) - mdl_period_start + 1)
      endp   <- as.integer(end_period(period)   - mdl_period_start + 1)
      return (list(startp = startp, endp = endp))
    },
    set_var = function(set_type, data, names, names_missing) {
      # General function used to update model data, constant adjustments,
      # fix values or fit targets.
      if (NCOL(data) == 0) {
        # TODO: warning?
        return (invisible(NULL))
      }
      if (is.null(names)) {
        if (names_missing) {
          stop(paste("Argument data has no colnames.",
                     "In that case, argument names should be specified"))
        } else {
          stop("names is null")
        }
      }  else {
        if (length(names) < NCOL(data)) {
          stop("The length of arument names is less than the number of columns of data")
        }
      }
      if (is.null(private$model_period)) stop(private$period_error_msg)
      data <- as.regts(data)
      shift <- private$get_period_indices(
        get_period_range(data))$startp
      if (!is.matrix(data)) {
        dim(data) <- c(length(data), 1)
      }
      if (is.integer(data) || !is.numeric(data)) {
        # make sure that data is a matrix of numeric values
        data <- apply(data, MARGIN = c(1,2), FUN = as.numeric)
      }

      if (set_type == 1) {
        lbls <- ts_labels(data)
        if (!is.null(lbls)) {
          names(lbls) <- names
          private$update_labels(lbls)
        }
      }
      .Call(set_c, set_type, private$model_index, data, names, shift)

      # TODO: report number of timeseries that have not been set
      return (invisible(self))
    },
    set_values_ = function(set_type, value, names, pattern, period) {
      value <- as.numeric(value)
      period <- as.period_range(period)
      nper <- nperiod(period)
      vlen <- length(value)
      if (vlen != 1 && vlen != nper) {
        stop(paste("Argument value should have length 1 or the same",
                   "length  as the number of periods"))
      }
      if (is.null(pattern) && is.null(names)) {
        names <- private$var_names
      } else if (is.null(names)) {
        names <- self$get_var_names(pattern)
      } else if (!is.null(pattern)) {
        names <- union(names, self$get_var_names(pattern))
      }
      nvar <- length(names)
      data <- regts(matrix(value, nrow = nper, ncol = nvar),
                    period = period, names = names)
      private$set_var(set_type, data, names, FALSE)
    },
    change_var_ = function(set_type, fun, names, pattern, period, ...) {
      period <- as.period_range(period)
      nper <- nperiod(period)
      if (is.null(pattern) && is.null(names)) {
        names <- private$var_names
      } else if (is.null(names)) {
        names <- self$get_var_names(pattern)
      } else if (!is.null(pattern)) {
        names <- union(names, self$get_var_names(pattern))
      }
      if (set_type == 1) {
        data <- self$get_data(names = names, period = period)
      } else if (set_type == 2) {
        data <- self$get_ca(names = names, period = period)
      }
      data <- fun(data, ...)
      private$set_var(set_type, data, names, FALSE)
    },
    get_variables = function(type, names, period) {
      # general function used to get model data or constant adjustments
      if (is.null(private$model_period)) stop(private$period_error_msg)
      period <- as.period_range(period)
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
      return(ret)
    },
    update_labels = function(labels) {
      names <- intersect(names(labels), private$var_names)
      private$labels[names] <- labels[names]
      return(invisible(NULL))
    },
    get_mws = function() {
      # Returns an mws object, containing all information
      # about the model that is not written to the mif file.
      if (!is.null(private$model_period)) {

        data <- self$get_data()
        # remove columns /rows with only NA from data
        # todo: skip leading/trailing rows with only NA
        data <- data[ , ! apply(is.na(data) , 2 , all), drop = FALSE]

        ca   <- self$get_ca()
        # remove columns with only 0 from ca
        # todo: skip leading/trailing columns with only 0
        if (!is.null(ca)) {
          ca <- ca[, !apply(ca == 0, 2, all), drop = FALSE]
        }

        # todo: rms values
      } else {
        data <- NULL
        ca   <- NULL
      }
      l <- list(labels = private$labels,
                model_period = private$model_period,
                solve_options = self$get_solve_options(),
                cvgcrit = .Call("get_cvgcrit_c", private$model_index, 0L),
                ftrelax = .Call("get_ftrelax_c", private$model_index),
                data = data, ca = ca,
                fix = self$get_fix(), fit = self$get_fit())
      return(structure(l, class="mws"))
    },
    set_mws = function(x) {
      # Update the model with the information in a mws
      # mws object, containing all information about the
      # model that is not written to the mif file.
      if (!inherits(x, "mws")) {
        stop("Error in set_mws: x is not an mws object")
      }
      private$labels <- x$labels
      do.call(self$set_solve_options, x$solve_options)
      .Call("set_cvgcrit_set_mws", private$model_index, x$cvgcrit)
      .Call("set_ftrelax_set_mws", private$model_index, x$ftrelax)
      if (!is.null(x$data)) {
        self$init_data(data = x$data, ca = x$ca)
        self$set_period(x$model_period)
        if (!is.null(x$fix)) {
          self$set_fix(x$fix)
        }
        if (!is.null(x$fit)) {
          self$set_fit(x$fit)
        }
      }
      return (invisible(self))
    },
    init_data_ = function(data_period) {
      # Sets the data period, initializes all model data
      # and constant adjustments, and removes fix values
      # and fit targets. The model data is initialized with NA,
      # constant adjustments with 0.
      private$data_period <- data_period
      fortran_period <- period_range(
        start_period(data_period) + private$maxlag,
        end_period(data_period)   - private$maxlead)
      private$fortran_period <- fortran_period
      # fortran_period is the data period without
      # lag and lead periods.
      p1 <- start_period(fortran_period)
      p2 <- end_period(fortran_period)
      start <- as.integer(c(get_year(p1), get_subperiod(p1)))
      end   <- as.integer(c(get_year(p2), get_subperiod(p2)))
      .Fortran("set_period_fortran",
               model_index = private$model_index,
               start = start, end = end,
               freq  = as.integer(fortran_period[3]), ier = 1L)
    },
    check_model_period = function(period) {
      if ((start_period(period) < start_period(private$fortran_period))  ||
          (end_period(period)   > end_period(private$fortran_period ))) {
        stop(paste0("The specified period (", period,
                    ") is not compatible with the data period (",
                    private$data_period, "). The period",
                    " should lie within the range ",
                    private$fortran_period, "."))
      }
      return(invisible(NULL))
    },
    check_options = function(options, type) {
      if (!is.list(options)) {
        stop(paste("The", type, "should be a named list"))
      }
      if (length(options) == 0) {
        return(invisible(NULL))
      }
      names <- names(options)
      if (is.null(names) || !is.na(Position(f = function(x) {x == ""}, names))) {
          stop(paste("The", type, "should be a named list"))
      }
      return(invisible(NULL))
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


