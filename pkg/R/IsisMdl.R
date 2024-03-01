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
#' the package library. It is also possible to directly create \code{IsisMdl} objects with functions
#' \code{\link{islm_mdl}} to create an ISLM model and \code{\link{ifn_mdl}}
#' to create another example model, the IFN model. The latter model is a model with
#' leads and can be solved with the Fair-Taylor-method.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @useDynLib isismdl read_mdl_c
#' @useDynLib isismdl write_mdl_c
#' @useDynLib isismdl get_max_lag_lead_c
#' @useDynLib isismdl get_var_names_c
#' @useDynLib isismdl get_eq_names_c
#' @useDynLib isismdl get_par_names_c
#' @useDynLib isismdl set_period_c
#' @useDynLib isismdl get_param_c
#' @useDynLib isismdl get_data_c
#' @useDynLib isismdl get_fix_fit_c
#' @useDynLib isismdl set_param_c
#' @useDynLib isismdl set_data_c
#' @useDynLib isismdl set_rms_c
#' @useDynLib isismdl get_rms_c
#' @useDynLib isismdl set_solve_opts_c
#' @useDynLib isismdl get_solve_opts_c
#' @useDynLib isismdl set_fit_opts_c
#' @useDynLib isismdl get_fit_opts_c
#' @useDynLib isismdl solve_c
#' @useDynLib isismdl filmdt_c
#' @useDynLib isismdl remove_mws_c
#' @useDynLib isismdl clear_fit_c
#' @useDynLib isismdl clear_fix_c
#' @useDynLib isismdl set_cvgcrit_c
#' @useDynLib isismdl set_cvgcrit_init_mws_c
#' @useDynLib isismdl get_cvgcrit_c
#' @useDynLib isismdl set_ftrelax_c
#' @useDynLib isismdl set_ftrelax_init_mws_c
#' @useDynLib isismdl get_ftrelax_c
#' @useDynLib isismdl set_eq_status_c
#' @useDynLib isismdl clone_mws_c
#' @useDynLib isismdl run_eqn_c
#' @useDynLib isismdl get_solve_status_c
#' @useDynLib isismdl set_dbgeqn_c
#' @useDynLib isismdl get_dbgeqn_c
#' @useDynLib isismdl get_jc_c
#' @useDynLib isismdl order_mdl_c
#' @useDynLib isismdl has_free_mws_c
#' @useDynLib isismdl get_simul_names_c
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
#' \item{\code{\link{copy}}}{Returns a deep copy of the \code{IsisMdl} object}
#'
#' \item{\code{\link{get_text}}}{Returns the textual representation of the model}
#'
#' \item{\code{\link{get_dep_struct}}}{Returns the dependency structure of the
#' model variables}
#'
#' \item{\code{\link{get_maxlag}}}{Returns the maximum lag}
#'
#' \item{\code{\link{get_maxlead}}}{Returns the maximum lead}
#'
#' \item{\code{\link{get_var_names}}}{Returns the names of the model
#' variables}
#'
#' \item{\code{\link{get_exo_names}}}{Returns the names of the exogenous model
#' variables}
#'
#' \item{\code{\link{get_endo_names}}}{Returns the names of the endogenous model
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
#' \item{\code{\link{set_param_values}}}{Sets the value of one or more model parameter}
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
#' \item{\code{\link{set_rms}}}{Sets one or more the rms values used in the fit procedure}
#'
#' \item{\code{\link{set_rms_values}}}{Sets the rms values used in the fit procedure}
#'
#' \item{\code{\link{set_solve_options}}}{Sets the solve options}
#'
#' \item{\code{\link{get_solve_options}}}{Returns the solve options}
#'
#' \item{\code{\link{set_fit_options}}}{Sets the options for the fit procedure}
#'
#' \item{\code{\link{get_fit_options}}}{Returns the options for the fit procedure}
#'
#' \item{\code{\link{set_debug_eqn}}}{Sets the debug equation option}
#'
#' \item{\code{\link{get_debug_eqn}}}{Returns the debug equation option}
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
#' \item{\code{\link{set_ftrelax}}}{Sets the Fair-Taylor relaxtion factors}
#'
#' \item{\code{\link{get_ftrelax}}}{Returns the Fair-Taylor relaxtion factors}
#'
#' \item{\code{\link{solve}}}{Solves the model}
#'
#' \item{\code{\link{get_solve_status}}}{Returns the status of the last model
#' solve attempt}
#'
#' \item{\code{\link{fill_mdl_data}}}{Calculates missing model
#' data from identities}
#'
#' \item{\code{\link{write_mdl}}}{Serializes the model object and writes it
#' to an RDS file}
#'
#' \item{\code{\link{order}}}{Orders the equations of the model}
#' }
#' @seealso \code{\link{isis_mdl}}, \code{\link{islm_mdl}}
#' and \code{\link{ifn_mdl}}
#'
IsisMdl <- R6Class("IsisMdl",
  public = list(

    initialize = function(serialized_mdl, mif_file, model_text,
                          silent = FALSE) {

      if (!missing(mif_file) && !missing(serialized_mdl)) {
        stop("Specify either argument mif_name or serialized_mdl, but not both")
      }
      if (missing(mif_file) && missing(serialized_mdl)) {
        stop("Specify either argument mif_name or serialized_mdl")
      }

      reorder_names <- 0L
      if (!missing(serialized_mdl)) {
        if (!inherits(serialized_mdl, "serialized_isismdl")) {
          stop("Argument serialized_mdl is not a serialized_isismdl object")
        }
        mif_file <- tempfile("mif")
        writeBin(serialized_mdl$mif_data, con = mif_file)

        if (serialized_mdl$version < "1.9.0") reorder_names <- 1L
      }

      has_free_mws <- .Call(has_free_mws_c)
      if (!has_free_mws) gc(verbose = FALSE)

      if (!silent) {
        cat("Reading mif file ...\n")
        if (reorder_names) {
          cat(paste0("\nModel names will be reordered because the IsisMdl ",
                     "object\nwas created with isismdl version < 1.9.0.\n\n"))
        }
        private$model_index <- .Call(read_mdl_c, mif_file, reorder_names)
        cat("\n")
      } else {
        output <- capture.output({
          private$model_index <- .Call(read_mdl_c, mif_file, reorder_names)
       })
      }
      if (!missing(serialized_mdl)) {
        unlink(mif_file)
      }

      # get maximum lag and lead
      ret <- .Call(get_max_lag_lead_c, model_index = private$model_index)
      private$maxlag <- ret[1]
      private$maxlead <- ret[2]
      private$var_names <- sort(.Call(get_var_names_c, "all",
                                 private$model_index))
      private$var_count <- length(private$var_names)
      private$labels    <- character(0)
      names(private$labels) <- character(0)
      if (!missing(serialized_mdl)) {
        private$init_mws(serialized_mdl$mws)
        private$model_text <- serialized_mdl$model_text
        if (!is.null(serialized_mdl$user_data)) {
          private$user_data <- serialized_mdl$user_data
        }
      } else {
        private$model_text <- model_text
      }
    },
    print = function(...) {
      cat(paste(class(self)[1], "object\n"))
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
      return(invisible(self))
    },
    finalize = function() {
      # remove_mws_c is unloaded if unloadNamespace() has been
      # called
      if (is.loaded("remove_mws_c", "isismdl")) {
        .Call("remove_mws_c", model_index = private$model_index)
      }
    },
    get_text = function() {
      return(private$model_text)
    },
    get_dep_struct = function() {
      return(get_dep_struct_internal(private$model_text, self$get_endo_names()))
    },
    get_maxlag = function() {
      return(private$maxlag)
    },
    get_maxlead = function() {
      return(private$maxlead)
    },
    get_var_names = function(pattern = ".*", type = c("all", "lags", "leads")) {
      type <- match.arg(type)
      names <- .Call(get_var_names_c, type, private$model_index)
      if (!missing(pattern)) {
        sel <- grep(pattern, names)
        names <- names[sel]
      }
      return(sort(names))
    },
    get_exo_names = function(pattern = ".*") {
      names <- setdiff(self$get_var_names(pattern), self$get_endo_names(pattern))
      return(sort(names))
    },
    get_endo_names = function(pattern = ".*",
                              type = c("all", "frml", "lags", "leads",
                                       "feedback", "endolead"),
                              status = c("active", "inactive", "all")) {
      type <- match.arg(type)
      status <- match.arg(status)

      if (type == "endolead") {
        warning(paste("Type 'endolead' is obsolete and has been ",
                      "replaced by 'leads'."))
        type <- "leads"
      }

      if (type == "all" || type == "lags") {
        names <- .Call(get_eq_names_c, private$model_index, status, 0L, 1L)
        if (type == "lags") {
          names <- intersect(names, self$get_var_names(type = "lags"))
        }
      } else {
        # Types 'frml', 'leads' and 'feedback'
        type_ <- if (type == "leads") "endolead" else type
        names <- .Call(get_var_names_c, type_, private$model_index)
        if (status != "all") {
          # If status != "all", select only activated or deactivated equations.
          endo_names_status <- .Call("get_eq_names_c", private$model_index,
                              status, 0L, 1L)
          names <- intersect(names, endo_names_status)
        }
      }
      if (!missing(pattern)) {
        sel <- grep(pattern, names)
        names <- names[sel]
      }
      return(sort(names))
    },
    get_simul_names = function() {
      sim_names <- .Call(get_simul_names_c, private$model_index)
      # Remove empty strings: get_simul_names_c returns an empty string for the
      # lhs variabes of inacvtive euations.
      sim_names <- sim_names[sim_names != ""]
      return(sim_names)
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
      return(sort(names))
    },
    get_eq_names = function(pattern = ".*",
                            status = c("all", "active", "inactive"),
                            order = c("sorted", "solve", "natural")) {
      status  <- match.arg(status)
      order <- match.arg(order)

      solve_order <- if (order == "solve") 1L else 0L
      names <- .Call("get_eq_names_c", private$model_index, status,
                     solve_order,  0L)

      if (!missing(pattern)) names <- grep(pattern, names, value = TRUE)

      if (order == "sorted") names <- sort(names)
      return(names)
    },
    set_debug_eqn = function(value) {
      if (!is.logical(value)) {
        stop("value should be logical")
      }
      .Call("set_dbgeqn_c", private$model_index, value)
      return(invisible(self))
    },
    get_debug_eqn = function() {
      return(.Call("get_dbgeqn_c", private$model_index))
    },
    init_data = function(data_period, data, ca) {
      if (missing(data_period) || is.null(data_period)) {
        if (!missing(data) && !is.null(data)) {
          # determine the data period from data and the model period (if known)
          if (is.null(private$model_period)) {
            data_period <- get_period_range(data)
          } else {
            p <- private$model_period
            p_data <- get_period_range(data)
            data_period <- period_range(
              min(start_period(p) - private$maxlag,
                  start_period(p_data)),
              max(end_period(p) + private$maxlead,
                  end_period(p_data)))
          }
        } else {
          # neither data_period nor data specified.
          if (is.null(private$data_period)) {
            stop(paste("If neither data_period nor data have been specified,",
                       "then the data period\nshould have been set before",
                       "with method init_data or set_period."))
          } else {
            data_period <- private$data_period
          }
        }
      } else {
        # data_period specified. Check if model_period is inside data_period
        data_period <- as.period_range(data_period)
        if (!is.null(private$model_period)) {
          mp <- private$model_period
          startp <- start_period(mp) - private$maxlag
          endp <- end_period(mp) + private$maxlead
          if (start_period(data_period) > startp ||
              end_period(data_period)  < endp) {
            p <- period_range(startp, endp)
            stop(paste0("The data period should include the range ",
                       as.character(p), "."))
          }
        }
      }

      private$init_data_(data_period)

      # if the model period has not been set, then determine the model
      # period from the data period
      if (is.null(private$model_period)) {
        startp <- start_period(data_period) + private$maxlag
        endp <- end_period(data_period) - private$maxlead
        if (endp < startp) {
          stop(paste("The data period is too short. It should contain at least",
                     private$maxlag + private$maxlead + 1, "periods"))
        }
        private$model_period <- period_range(startp, endp)
      }

      #
      # now update data and ca
      #
      if (!missing(data) && !is.null(data)) {
        if (is.null(colnames(data))) {
          stop("data should be a timeseries with colnames")
        }
        data <- private$convert_data_internal(data)
        if (is.null(data)) return(invisible(self))
        private$set_data_(private$data_type, data)
      }
      if (!missing(ca) && !is.null(ca) && NCOL(ca) > 0) {
        if (is.null(colnames(ca))) {
          stop("ca should be a timeseries with colnames")
        }
        ca <- private$convert_data_internal(ca)
        if (is.null(ca)) return(invisible(self))
        private$set_data_(private$ca_type, ca)
      }
      return(invisible(self))
    },
    set_period = function(period) {
      period <- as.period_range(period)
      if (is.na(period[1]) || is.na(period[2])) {
        stop("period should have a lower and upper bound")
      }
      if (is.null(private$data_period)) {
        data_period <- period_range(
          start_period(period) - private$maxlag,
          end_period(period)   + private$maxlead)
        private$init_data_(data_period)
      } else  {
        private$check_model_period(period)
      }
      private$model_period <- period
      return(invisible(self))
    },
    get_period = function() {
      return(private$model_period)
    },
    get_data_period = function() {
      return(private$data_period)
    },
    set_param = function(p, name_err = "warn") {
      p <- as.list(p)

      if (is.null(names(p))) {
        stop("Argument p has no names")
      }

      names <- private$get_names_(private$param_type, names = names(p),
                                  name_err = name_err)

      npar <- length(names)
      if (npar == 0) return(invisible(self))
      p <- p[names]

      # check if the list contains any non-numeric elements
      is_not_num <- unlist(lapply(p, FUN = function(x) !is.numeric(x)))
      if (any(is_not_num)) {
        no_numeric <- names(p)[is_not_num]
        is_or_are <- if (length(no_numeric) == 1) "is" else "are"
        stop(concat_names(no_numeric), " ", is_or_are, " not numeric")
      }
      # convert integer list elements to numeric
      p <- lapply(p, as.numeric)

      nset <- .Call("set_param_c", model_index = private$model_index, p)
      stopifnot(nset == npar)
      return(invisible(self))
    },
    set_param_values = function(value, names, pattern) {
      if (is.integer(value) || (is.logical(value) && all(is.na(value)))) {
        value <- as.numeric(value)
      } else if (!is.numeric(value)) {
        stop("Argument 'value' should be a numeric vector")
      }
      names <- private$get_names_(private$param_type, names, pattern)
      nparam <- length(names)
      p <- rep(list(value), nparam)
      base::names(p) <- names
      nset <- .Call("set_param_c", model_index = private$model_index, p)
      stopifnot(nset == nparam)
      return(invisible(self))
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
      return(.Call("get_param_c", model_index = private$model_index,
                    names = names))
    },
    set_data = function(data, names, upd_mode = c("upd", "updval"), fun,
                        name_err = "silent") {
      upd_mode <- match.arg(upd_mode)
      data <- private$convert_data_internal(data, names)
      if (is.null(data)) return(invisible(self))
      names <- private$get_names_(private$data_type, names = colnames(data),
                                  name_err = name_err)
      if (length(names) == 0) return(invisible(self))
      data <- data[, names, drop = FALSE]
      return(private$set_data_(private$data_type, data, upd_mode, fun))
    },
    set_ca = function(data, names, upd_mode = c("upd", "updval"), fun,
                      name_err = "silent") {
      upd_mode <- match.arg(upd_mode)
      data <- private$convert_data_internal(data, names)
      if (is.null(data)) return(invisible(self))
      names <- private$get_names_(private$ca_type, names = colnames(data),
                                  name_err = name_err)
      if (length(names) == 0) return(invisible(self))
      data <- data[, names, drop = FALSE]
      return(private$set_data_(private$ca_type, data, upd_mode, fun))
    },
    set_fix = function(data, names, upd_mode = c("upd", "updval"),
                       name_err = "silent") {
      upd_mode <- match.arg(upd_mode)
      data <- private$convert_data_internal(data, names)
      if (is.null(data)) return(invisible(self))
      names <- private$get_names_(private$fix_type, names = colnames(data),
                                  name_err = name_err)
      if (length(names) == 0) return(invisible(self))
      data <- data[, names, drop = FALSE]
      return(private$set_data_(private$fix_type, data, upd_mode))
    },
    set_fit = function(data, names, upd_mode = c("upd", "updval"),
                       name_err = "silent") {
      upd_mode <- match.arg(upd_mode)
      data <- private$convert_data_internal(data, names)
      if (is.null(data)) return(invisible(self))
      names <- private$get_names_(private$fit_type, names = colnames(data),
                                  name_err = name_err)
      if (length(names) == 0) return(invisible(self))
      data <- data[, names, drop = FALSE]
      return(private$set_data_(private$fit_type, data, upd_mode))
    },
    get_data = function(pattern, names,
                        period = private$data_period) {
      return(private$get_data_(private$data_type, names, pattern, period))
    },
    get_ca = function(pattern, names, period = private$data_period) {
      return(private$get_data_(private$ca_type, names, pattern, period))
    },
    get_fix = function() {
      return(private$get_fix_fit(type = "fix"))
    },
    get_fit = function() {
      return(private$get_fix_fit(type = "fit"))
    },
    set_values = function(value, names, pattern, period = private$data_period) {
      return(private$set_values_(private$data_type, value, names, pattern,
                                 period))
    },
    set_ca_values = function(value, names, pattern,
                             period = private$data_period) {
      return(private$set_values_(private$ca_type, value, names, pattern,
                                 period))
    },
    set_fix_values = function(value, names, pattern,
                              period = private$data_period) {
      return(private$set_values_(private$fix_type, value, names, pattern,
                                 period))
    },
    set_fit_values = function(value, names, pattern,
                              period = private$data_period) {
      return(private$set_values_(private$fit_type, value, names, pattern,
                                 period))
    },
    change_data = function(fun, names, pattern, period = private$data_period,
                           ...) {
      return(private$change_data_(private$data_type, fun, names, pattern,
                                 period, ...))
    },
    change_ca = function(fun, names, pattern, period = private$data_period,
                         ...) {
      return(private$change_data_(private$ca_type, fun, names, pattern,
                                 period, ...))
    },
    set_rms = function(values, name_err = "warn") {
      if (is.integer(values) || (is.logical(values) && all(is.na(values)))) {
        values[] <- as.numeric(values)
      } else if (!is.numeric(values)) {
         stop("Argument values is not a numeric vector")
      }
      if (is.null(names(values))) {
        stop("Argument values is not a named numeric vector")
      }
      names <- private$get_names_(private$rms_type, names = names(values),
                                  name_err = name_err)
      if (length(names) == 0) return(invisible(self))
      values <- values[names]
      .Call(set_rms_c, private$model_index, values)
      return(invisible(self))
    },
    set_rms_values = function(value, names, pattern) {
      err_msg <- "Argument 'value' should be a scalar numeric"
      if (length(value) != 1) stop(err_msg)
      if (is.integer(value) || (is.logical(value) && is.na(value))) {
        value <- as.numeric(value)
      } else if (!is.numeric(value)) {
        stop(err_msg)
      }
      names <- private$get_names_(private$rms_type, names, pattern)
      if ((n <- length(names)) > 0) {
          values <- rep(value, n)
          base::names(values) <- names
          .Call(set_rms_c, private$model_index, values)
      }
      return(invisible(self))
    },
    get_rms = function() {
      values <- .Call(get_rms_c, private$model_index)
      if (is.null(values)) {
        return(numeric(0))
      } else {
        names(values) <- .Call(get_var_names_c, "frml", private$model_index)
        values <- values[!is.na(values)]
        values <- values[values != 0]
        if (length(values) == 0) {
          return(numeric(0))
        } else {
          return(values[order(names(values))])
        }
      }
    },
    fix_variables = function(names, pattern, period = self$get_period()) {
      period <- private$convert_period_arg(period)
      names <- private$get_names_(private$fix_type, names, pattern)
      if (length(names) == 0) {
        return(NULL)
      }
      js <- private$get_period_indices(period)
      fix_data <- .Call("get_data_c", type = private$data_type,
                    model_index = private$model_index,
                    names = names, jtb = js$startp, jte = js$endp)
      fix_data <- regts(fix_data, start = start_period(period), names = names)
      if (any(is.na(fix_data))) {
        na_names <- colnames(fix_data)[apply(fix_data,
                                FUN = function(x) any(is.na(x)), MARGIN = 2)]
        na_names_text <- paste(na_names, collapse = " ")
        na_names_lines <- strwrap(na_names_text, width = 80)
        na_names_text <- paste(na_names_lines, collapse = "\n")
        stop(paste0("The following variables can't be fixed in ", period,
                   "\nbecause of NA values in the model data:\n",
                   na_names_text), ".")
      }
      self$set_fix(fix_data)
      return(invisible(self))
    },
    set_solve_options = function(mode, fbstart, maxiter, maxjacupd, rlxspeed,
                                 rlxmin, rlxmax, cstpbk, cnmtrx, xrelax,
                                 xmaxiter, xupdate, dbgopt, erropt,
                                 report, ratreport, ratreport_rep,
                                 ratfullreport_rep, bktmax, xtfac,
                                 svdtest_tol) {

      # create a list of supplied options
      names <- names(match.call()[-1])
      options <- lapply(names, FUN = function(x) eval(parse(text = x)))
      names(options) <- names
      .Call("set_solve_opts_c", private$model_index, options)
      return(invisible(self))
    },
    set_fit_options = function(maxiter, cvgabs, mkdcrt, cvgrel, zero_ca,
                               warn_ca, accurate_jac, zealous, scale_method,
                               warn_zero_row, warn_zero_col, chkjac, report,
                               dbgopt, svdtest_tol) {
      names <- names(match.call()[-1])
      options <- lapply(names, FUN = function(x) eval(parse(text = x)))
      names(options) <- names
      .Call("set_fit_opts_c", private$model_index, options)
      return(invisible(self))
    },
    solve = function(period = private$model_period, options = list(),
                     fit_options = list()) {
      "Solve the model for the specified period"
      if (is.null(private$model_period)) stop(private$period_error_msg)
      private$check_options(options, type = "solve_options")
      private$check_options(options, type = "fit_options")
      period <- private$convert_period_arg(period, data_period = FALSE)
      private$check_model_period(period)
      js <- private$get_period_indices(period)
      .Call("solve_c", model_index = private$model_index,
            jtb = js$startp, jte = js$endp, options,
            fit_options)
      return(invisible(self))
    },
    get_solve_status = function() {
      return(.Call("get_solve_status_c", model_index = private$model_index))
    },
    fill_mdl_data = function(period = private$data_period,
                             report = c("period", "minimal", "no")) {
      # Calculates missing model data from identities.
      report <- match.arg(report)
      if (is.null(private$model_period)) stop(private$period_error_msg)
      period <- private$convert_period_arg(period)
      js <- private$get_period_indices(period)
      .Call("filmdt_c", model_index = private$model_index,
            jtb = js$startp, jte = js$end,
            report = report)
      return(invisible(self))
    },
    run_eqn = function(pattern, names, period = private$data_period,
                       solve_order, forwards = TRUE,
                       update_mode = c("upd", "updval"),
                       by_period = FALSE) {
      "Run model equations"

      update_mode <- match.arg(update_mode)
      period <- private$convert_period_arg(period)
      if (!is.logical(forwards) || length(forwards) != 1 || is.na(forwards)) {
        stop("Argument 'forwards' should be a TRUE or FALSE")
      }
      if (!is.logical(by_period) || length(by_period) != 1 ||
          is.na(by_period)) {
        stop("Argument 'by_period' should be a TRUE or FALSE")
      }

      if (missing(names)) {
        order <- if (missing(solve_order) || solve_order) "solve" else "natural"
        eq_names <- self$get_eq_names(pattern = pattern, status = "active",
                                      order = order)
      } else {
        if (!missing(pattern)) {
          stop("Only one of arguments 'pattern' and 'names' can be specified")
        }
        if (missing(solve_order)) solve_order <- FALSE
        eq_names <- private$get_eq_names_(TRUE, names, pattern, solve_order)
      }

      if (length(eq_names) > 0) {
        eqnums <- match(eq_names, self$get_eq_names(order = "natural"))
        js <- private$get_period_indices(period)

        if (forwards) {
          jtb <- js$startp
          jte <- js$end
        } else {
          jtb <- js$end
          jte <- js$startp
        }
        updval <- if (update_mode == "updval") 1L else 0L
        .Call("run_eqn_c", private$model_index, eqnums = as.integer(eqnums),
              jtb_ = jtb, jte_ = jte, updval__ = updval, by_period__ = by_period)
      }
      return(invisible(self))
    },
    get_solve_options = function() {
      "Returns the default solve options"
      return(.Call("get_solve_opts_c",
                    model_index = private$model_index))
    },
    get_fit_options = function() {
      "Returns the default fit options"
      return(.Call("get_fit_opts_c",
                   model_index = private$model_index))
    },
    get_cvgcrit = function() {
      values <- .Call("get_cvgcrit_c", private$model_index, 1L)
      names(values) <- self$get_var_names()
      return(values)
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
      return(invisible(self))
    },
    set_ftrelax = function(value, pattern, names) {
      "Sets the Fair-Taylor relaxation criterion."
      if (!(is.numeric(value) || is.logical(value)) || length(value) != 1) {
        stop("value should be a single numerical value")
      }
      value <- as.numeric(value)
      # TODO: check for valid value?
      # TODO: use get_names_ approach
      if (missing(pattern) && missing(names)) {
        names <- self$get_endo_names(type = "leads", status = "all")
      } else {
        if (!missing(names)) {
          correct_names <- self$get_endo_names(type = "leads", status = "all")
          check_names(names, correct_names = correct_names,
                      type = "endogenous lead")
        } else {
          names <- character(0)
        }
        if (!missing(pattern)) {
          pvars <- self$get_endo_names(pattern, type = "leads", status = "all")
          names <- union(pvars, names)
        }
      }
      .Call("set_ftrelax_c", private$model_index, names, value)
      return(invisible(self))
    },
    get_ftrelax = function() {
      "Returns the Fair-Taylor relaxtion factors"
      values <- .Call("get_ftrelax_c", private$model_index)
      names(values) <- .Call(get_var_names_c, "endolead", private$model_index)
      sorted_names <- sort(names(values))
      return(values[sorted_names])
    },
    set_eq_status = function(status = c("active", "inactive"),
                             pattern, names) {
      "Activate or deactivate equations"
      status <- match.arg(status)
      names <- private$get_eq_names_(FALSE, names, pattern, solve_order = FALSE)
      if (length(names) > 0) {
        .Call("set_eq_status_c", private$model_index, names, status)
      }
      return(invisible(self))
    },
    get_last_solve_period = function() {
      jc <- .Call("get_jc_c", private$model_index)
      if (jc != -1) {
        return(start_period(private$fortran_period)  + jc  - 1)
      } else {
        return(NULL)
      }
    },
    write_mdl = function(file) {
      saveRDS(self$serialize(), file)
      return(invisible(self))
    },
    serialize = function() {
      mif_file <- tempfile()
      .Call("write_mdl_c", mif_file, private$model_index)
      size <- file.info(mif_file)$size
      mif_data <- readBin(mif_file, what = "raw", n = size)
      unlink(mif_file)
      return(structure(list(version = packageVersion("isismdl"),
                            model_text = private$model_text,
                            mif_data = mif_data, mws = private$get_mws(),
                            user_data = private$user_data),
                       class = "serialized_isismdl"))
    },
    clear_fit = function() {
      .Call("clear_fit_c", model_index = private$model_index)
      return(invisible(self))
    },
    clear_fix = function() {
      .Call("clear_fix_c", model_index = private$model_index)
      return(invisible(self))
    },
    order = function(orfnam = NULL, silent = FALSE) {
      if (!is.null(orfnam)) {
        if (!is.character(orfnam)) {
          stop("orfnam is not an character string")
        }
        if (file.exists(orfnam)) {
          file.remove(orfnam)
        }
      }
      call_order_mdl_c <- function() {
        .Call(order_mdl_c, model_index = private$model_index,
              orfnam = orfnam)
        return(invisible())
      }
      if (silent) {
        output <- capture.output({
          call_order_mdl_c()
        })
      } else {
        call_order_mdl_c()
        cat("\n")
      }
      return(invisible(self))
    },
    copy = function() {
      return(self$clone(deep = TRUE))
    },
    set_user_data = function(user_data, ...) {
      if (!missing(user_data) && !is.null(user_data)) {
        if (!is.list(user_data) || is.null(names(user_data))) {
          stop("Argument 'user_data' must be a named list")
        }
        private$user_data <- user_data
      }
      dot_args <- list(...)
      private$user_data[names(dot_args)] <- dot_args

      # verwijder alle NULL elementen
      is_null <- sapply(private$user_data, FUN = is.null)
      private$user_data <- private$user_data[!is_null]
    },
    get_user_data = function(key) {
      if (missing(key) || is.null(key)) {
        return(private$user_data)
      } else {
        if (!is.character(key)) stop("Argument 'key' must be a character")
        user_data_keys <- names(private$user_data)
        if (length(key) == 1) {
          if (!key %in% user_data_keys) {
            stop("'", key, "' is not a user data key")
          }
          return(private$user_data[[key]])
        } else {
          unknown_keys <- setdiff(key, user_data_keys)
          if (length(unknown_keys) > 0) {
            unknown_keys <- paste0("'", unknown_keys, "'")
            stop("The following keys are not present in user data:\n",
                 paste(unknown_keys, collapse = ", "))
          }
          return(private$user_data[key])
        }
      }
    }
  ),
  private = list(
    model_text = NA_character_,
    maxlag = NA_integer_,
    maxlead = NA_integer_,
    model_period = NULL,
    data_period = NULL,
    fortran_period = NULL,
    model_index = NA_integer_,
    var_names = NULL,
    var_count = NA_integer_,
    labels = NULL,
    period_error_msg = paste("The model period has not been set.",
                             "Set the model period with set_period() or",
                             "init_data()."),
    data_type = 1L,
    ca_type = 2L,
    fix_type = 3L,
    fit_type = 4L,
    rms_type = 5L,
    param_type = 6L,

    user_data = list(),

    deep_clone = function(name, value) {
      if (name == "model_index") {
        has_free_mws <- .Call("has_free_mws_c")
        if (!has_free_mws) gc(verbose = FALSE)
        return(.Call("clone_mws_c", model_index = value))
      } else {
        return(value)
      }
    },
    get_period_indices = function(period, extended = TRUE) {
      startp <- start_period(period)
      endp <- end_period(period)
      mdl_period_start <- start_period(private$fortran_period)
      startp <- as.integer(startp - mdl_period_start + 1)
      endp   <- as.integer(endp   - mdl_period_start + 1)
      return(list(startp = startp, endp = endp))
    },
    get_data_ = function(type, names, pattern, period) {
      # general function used to get model data or constant adjustments
      if (is.null(private$model_period)) stop(private$period_error_msg)
      period <- private$convert_period_arg(period)
      names <- private$get_names_(type, names, pattern)
      if (length(names) == 0) {
        return(NULL)
      }
      js <- private$get_period_indices(period)
      data <- .Call("get_data_c", type = type,
                    model_index = private$model_index,
                    names = names, jtb = js$startp, jte = js$endp)
      ret <- regts(data, start = start_period(period), names = names)
      if (length(private$labels) > 0) {
        lbls <- private$labels
        if (type == private$ca_type) {
          lbls[] <- paste(lbls, "(constant adjustment)")
        }
        ret <- update_ts_labels(ret, lbls)
      }
      return(ret)
    },
    set_data_ = function(set_type, data, upd_mode = "upd", fun) {
      # internal function to transform data to the model workspace. This function
      # assume that data is already in the correct from:
      # 1) A multivariate ts with colnames.
      # 2) Numerical values

      names <- colnames(data)

      if (set_type == private$data_type) {
        lbls <- ts_labels(data)
        if (!is.null(lbls)) {
          names(lbls) <- names
          private$update_labels(lbls)
        }
      }

      # check for inactive equations
      if (set_type == private$fix_type || set_type == private$fit_type) {
        if (set_type == private$fix_type) {
          vartype <- "frml"
          msg <- "fixed"
        } else {
          vartype <- "all"
          msg <- "used as fit targets"
        }
        inactive <- intersect(names, self$get_endo_names(type = vartype,
                                                         status = "inactive"))
        if (length(inactive) > 0) {
          if (length(inactive) == 1) {
            stop(paste("Variable", inactive, "is inactive and cannot be", msg))
          } else {
            stop(paste("The variables", paste(inactive, collapse = " "),
                       "are inactive and cannot be", msg))
          }
        }
      }

      if (!missing(fun)) {
        if (!is.function(fun)) {
          stop("fun is not a function")
        }
        p <- range_intersect(get_period_range(data), private$data_period)
        if (is.null(p)) {
          return(invisible(self))
        }
        if (set_type == private$data_type) {
          names <- intersect(names, self$get_var_names())
          old_data <- self$get_data(period = p, names = names)
        } else {
          names <- intersect(names,
                             self$get_endo_names(type = "frml", status = "all"))
          old_data <- self$get_ca(period = p, names = names)
        }
        if (length(names) == 0) {
          return(invisible(self))
        }
        new_data <- data[p, names]
        data <- fun(old_data, new_data)
        error <- !is.finite(data) & !is.na(old_data) & !is.na(new_data)
        if (any(error)) {
          warning("Numerical problem when evaluating fun")
        }
      }

      # finally transfer new data to the model workspace
      shift <- private$get_period_indices(get_period_range(data))$startp
      .Call(set_data_c, set_type, private$model_index, data, names, shift,
            upd_mode)

      return(invisible(self))
    },
    get_names_ = function(type, names, pattern,
                          name_err = c("stop", "warn", "silent")) {
      # This function selects model variable or parameter names from names
      # and pattern. It gives an error if names contain any invalid name for the
      # specified type of model variable.
      name_err <- match.arg(name_err)
      if (type %in% c(private$fix_type, private$ca_type, private$rms_type)) {
        var_type <- "frml"
        all_names <- self$get_endo_names(type = "frml", status = "all")
      } else if (type == private$fit_type) {
        var_type <- "endo"
        all_names <- self$get_endo_names(status = "all")
      } else if (type == private$param_type) {
        var_type <- "param"
        all_names <- self$get_par_names()
      } else {
        var_type <- "endo_exo"
        all_names <- self$get_var_names()
      }

      type_texts <- c(endo = "endogenous variable",
                      frml = "frml variable",
                      endo_exo  = "model variable",
                      param = "parameter")
      type_text <- type_texts[var_type]

      if (!missing(names)) {
        if (name_err != "silent") {
          check_names(names, correct_names = all_names,
                      type = type_text, is_warning = name_err == "warn")
        }
        names <- intersect(names, all_names)
      }
      if (missing(pattern) && missing(names)) {
        names <- all_names
      } else if (!missing(pattern)) {
        sel <- grep(pattern, all_names)
        if (length(sel) == 0) {
          warning("There are no ", type_text, "s that match pattern '",
                  pattern, "'.")
        }
        pattern_names <- all_names[sel]
        if (!missing(names)) {
          names <- union(pattern_names, names)
        } else {
          names <- pattern_names
        }
      }
      return(names)
    },
    get_eq_names_ = function(active, names, pattern, solve_order) {
      # This function selects equations names from specified
      # names and/or a pattern.  Used internally by methods
      # set_eq_status and run_eqn.
      # If solve_order == TRUE, then the equation names are returned in
      # solution order. Otherwise, the equations specified with a pattern are
      # defined in natural ordering (the order in which the equations are
      # stored in the model), and the specified names are returned in the
      # original ordering of the names.
      # The function gives an error if names contains any invalid name for
      # the specified type of equation.

      names_specified <- !missing(names)
      pattern_specified <- !missing(pattern)

      status <- if (active) "active" else "all"

      # Get all equation names with the sepcified status
      # Use order = "natural" because we don't care about the order here
      # and because sorting the equations can take a lot of time
      # for large models.
      order <- if (solve_order) "solve" else "natural"
      all_names <- self$get_eq_names(status = status, order = order)

      type_text <- if (active) " active " else " "

      # check supplied names, give an error if an invalid name has been
      # specified
      if (names_specified) {
        error_vars <- setdiff(names, all_names)
        if (length(error_vars) > 0) {
          error_vars <- paste0("\"", error_vars, "\"")
          if (length(error_vars) == 1) {
            stop(error_vars, " is not an", type_text, "equation.")
          } else {
            stop("The following names are no", type_text, "equations: ",
                        paste(error_vars, collapse = ", "), ".")
          }
        }
      }
      if (!(names_specified || pattern_specified)) {
        names <- all_names
      } else if (pattern_specified) {
        sel <- grep(pattern, all_names)
        if (length(sel) == 0) {
          warning("There are no", type_text, "equations that match pattern '",
                  pattern, "'.")
        }
        pattern_names <- all_names[sel]
        if (names_specified) {
          names <- union(pattern_names, names)
        } else {
          names <- pattern_names
        }
      }

      # reorder names, this is only necessary if argument names has been
      # specified (otherwise the variables have  already the correct order)
      if (names_specified && solve_order) {
        names <- all_names[all_names %in% names]
      }
      return(names)
    },
    set_values_ = function(set_type, value, names, pattern, period) {

      period <- private$convert_period_arg(period)
      if (is.null(range_intersect(period, private$data_period))) {
        warning(sprintf(paste("Specified period (%s) is completely outside the",
                              "data period (%s)."), period,
                        private$data_period))
        return(invisible(self))
      }

      if (is.integer(value) || (is.logical(value) && all(is.na(value)))) {
        value <- as.numeric(value)
      } else if (!is.numeric(value)) {
        stop("Argument 'value' is not a numeric vector")
      }
      nper <- nperiod(period)
      vlen <- length(value)
      if (vlen != 1 && vlen != nper) {
        stop(paste("Argument value should have length 1 or the same",
                   "length as the number of periods"))
      }
      names <- private$get_names_(set_type, names, pattern)
      names <- unique(names)
      nvar <- length(names)
      data <- regts(matrix(value, nrow = nper, ncol = nvar), period = period,
                    names = names)
      private$set_data_(set_type, data)
      return(invisible(self))
    },
    change_data_ = function(set_type, fun, names, pattern, period, ...) {
      period <- private$convert_period_arg(period)
      if (is.null(range_intersect(period, private$data_period))) {
        warning(sprintf(paste("Specified period (%s) is completely outside the",
                              "data period (%s)."), period,
                        private$data_period))
        return(invisible(self))
      }
      if (!is.function(fun)) {
        stop("Argument 'fun' is not a function.")
      }
      nper <- nperiod(period)
      names <- private$get_names_(set_type, names, pattern)
      names <- unique(names)
      if (set_type == private$data_type) {
        data <- self$get_data(names = names, period = period)
      } else if (set_type == private$ca_type) {
        data <- self$get_ca(names = names, period = period)
      }
      for (c in seq_len(ncol(data))) {
        fun_result <- fun(as.numeric(data[, c]), ...)
        result_len <- length(fun_result)
        if (result_len != 1 && result_len != nper) {
          stop(sprintf(paste("The function result has length %d but should have",
                             "length 1 or %d."), result_len, nper))
        }
        data[, c] <- fun_result
      }
      private$set_data_(set_type, data)
      return(invisible(self))
    },
    get_fix_fit = function(type) {
      # general function for getting fix or fit values
      ret <- .Call("get_fix_fit_c", type = type,
                   model_index = private$model_index)
      if (!is.null(ret)) {
        ret <- regts(ret[[2]], start = start_period(private$fortran_period)
                     + ret[[1]] - 1, names = ret[[3]])
        ret <- ret[, sort(colnames(ret)), drop = FALSE]
        if (length(private$labels) > 0) {
          ret <- update_ts_labels(ret, private$labels)
        }
      }
      return(ret)
    },
    update_labels = function(labels) {
      names <- intersect(names(labels), private$var_names)
      private$labels[names] <- labels[names]
      private$labels <- private$labels[order(names(private$labels))]
      return(invisible(NULL))
    },
    get_mws = function() {
      # Returns an mws object, containing all information
      # about the model that is not written to the mif file.
      if (!is.null(private$model_period)) {

        data <- self$get_data()
        # remove columns /rows with only NA from data
        data <- data[, ! apply(is.na(data) , 2 , all), drop = FALSE]
        if (ncol(data) > 0) {
          # remove leading and trailing rows with only NAs
          data <- na_trim(data)
        } else {
          data <- NULL
        }
        ca   <- self$get_ca()

        if (!is.null(ca)) {
          # remove columns with only 0 from ca
          ca <- ca[, !apply(ca == 0, 2, all), drop = FALSE]
          # remove leading and trailing rows with only zero
          if (ncol(ca) > 0) ca <- zero_trim(ca)
        }

      } else {
        data <- NULL
        ca   <- NULL
      }
      l <- list(labels = private$labels,
                model_period = private$model_period,
                data_period = private$data_period,
                debug_eqn = self$get_debug_eqn(),
                solve_options = self$get_solve_options(),
                fit_options = self$get_fit_options(),
                cvgcrit = .Call("get_cvgcrit_c", private$model_index, 0L),
                ftrelax = .Call("get_ftrelax_c", private$model_index),
                jc = .Call("get_jc_c", private$model_index),
                data = data, ca = ca,
                fix = self$get_fix(), fit = self$get_fit(),
                rms = self$get_rms())
      return(structure(l, class = "mws"))
    },
    init_mws = function(x) {
      # Initialize the mws with information in a mws
      # object, containing all information about the
      # model that is not written to the mif file.
      # Only used via read_mdl.
      if (!inherits(x, "mws")) {
        stop("Error in init_mws: x is not an mws object")
      }
      private$labels <- x$labels
      self$set_debug_eqn(x$debug_eqn)
      do.call(self$set_solve_options, x$solve_options)
      do.call(self$set_fit_options, x$fit_options)
      .Call("set_cvgcrit_init_mws_c", private$model_index, x$cvgcrit)
      .Call("set_ftrelax_init_mws_c", private$model_index, x$ftrelax)
      if (!is.null(x$model_period)) {
        private$model_period <- x$model_period
        self$init_data(data_period = x$data_period, data = x$data, ca = x$ca)
        if (!is.null(x$fix) && ncol(x$fix) > 0) {
          self$set_fix(x$fix)
          # x$data should be transferred to the model data again after
          # set_fix() has been called, because set_fix also transfers the
          # fix values to the model data:
          private$set_data_(private$data_type,
                            x$data[, colnames(x$fix), drop = FALSE])
        }
        if (!is.null(x$fit)) {
          self$set_fit(x$fit)
        }
        if (length(x$rms) > 0) {
          self$set_rms(x$rms)
        }

        # jc (last period solved, part of the mws since version 1.4)
        if (!is.null(x$jc)) {
          .Call("set_jc_c", model_index = private$model_index,
                jc = x$jc)
        }
      }
      return(invisible(self))
    },
    init_data_ = function(data_period) {
      # Sets the data period, initializes all model data
      # and constant adjustments, and removes fix values
      # and fit targets. The model data is initialized with NA,
      # constant adjustments with 0.
      private$data_period <- data_period
      startp <- start_period(data_period) + private$maxlag
      endp <- end_period(data_period) - private$maxlead
      if (startp > endp) {
        stop(paste("The data period is too short. It should contain at least",
                   private$maxlag + private$maxlead + 1, "periods"))
      }
      new_fortran_period <- period_range(startp, endp)

      # update jc
      if (!is.null(private$fortran_period)) {
        shift <- start_period(new_fortran_period) -
                 start_period(private$fortran_period)
        old_jc <- .Call("get_jc_c", private$model_index)
        new_jc <- as.integer(old_jc - shift)
        if (new_jc < 1 || new_jc > nperiod(new_fortran_period)) {
          # last solve period outside range of fortran period.
          new_jc <- -1L
        }
        .Call("set_jc_c", model_index = private$model_index, jc = new_jc)
      }

      private$fortran_period <- new_fortran_period

      # fortran_period is the data period without lag and lead periods.
      start <- as.integer(c(get_year(startp), get_subperiod(startp)))
      end   <- as.integer(c(get_year(endp), get_subperiod(endp)))
      ierr <- .Call("set_period_c",
               model_index = private$model_index,
               start = start, end = end,
               freq  = as.integer(private$fortran_period[3]))

      return(invisible(NULL))
    },
    check_model_period = function(period) {

      if (frequency(period) != frequency(private$data_period)) {
        stop(paste0("The specified period (", period,
                    ") has a different frequency than the data period (",
                    private$data_period, ")."))
      }

      if ((start_period(period) < start_period(private$fortran_period))  ||
          (end_period(period)   > end_period(private$fortran_period))) {
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
      if (is.null(names) || !is.na(Position(f = function(x) x == "", names))) {
          stop(paste("The", type, "should be a named list"))
      }
      return(invisible(NULL))
    },
    convert_period_arg = function(period, data_period = TRUE) {
      if (is.null(private$model_period)) {
        stop(private$period_error_msg)
      }
      period <- as.period_range(period)
      period <- change_frequency(period, frequency(private$data_period))

      if (data_period) {
        defaultp <- private$data_period
      } else {
        defaultp <- private$model_period
      }

      startp <- start_period(period)
      if (is.null(startp)) {
        startp <- start_period(defaultp)
      }
      endp <- end_period(period)
      if (is.null(endp)) {
        endp <- end_period(defaultp)
      }
      return(period_range(startp, endp))
    },
    convert_data_internal = function(data, names) {
      # Used by set_data and set_fit: checks the period range of data and
      # selects the appropriate period. Also converts data to a matrix ts with
      # colnames if necessary.

      if (is.null(private$model_period)) stop(private$period_error_msg)

      if (!inherits(data, "ts")) {
        # we use inherits and not is.ts, because is.ts returns FALSE if
        # length(x) == 0
        stop("Argument data is not a timeseries object")
      }

      if (NCOL(data) == 0) return(NULL)

      data <- as.regts(data)

      if (frequency(data) != frequency(private$data_period)) {
        stop(paste0("The frequency of data does not agree with the data",
                    " period ", as.character(private$data_period), "."))
      }

      per <- range_intersect(get_period_range(data), private$data_period)
      if (is.null(per)) return(NULL)
      data <- data[per]

      # convert data to a matrix if necessary
      if (!is.matrix(data)) {
        dim(data) <- c(length(data), 1)
      }

      if (!missing(names)) {
        if (is.null(names)) {
          stop("names is null")
        } else if (length(names) < NCOL(data)) {
          stop(paste("The length of argument names is less than the number of",
                     "columns of data"))
        }
        colnames(data) <- names
      } else if (is.null(colnames(data))) {
        stop(paste("Argument data has no colnames.",
                   "In that case, argument names should be specified"))
      } else {
        names <- colnames(data)
      }

      # check for duplicate names
      if (anyDuplicated(names)) {
        dupl <- duplicated(names)
        data <- data[, !dupl, drop = FALSE]
        warning(sprintf(paste("Data contains duplicate names. The first column",
                              "is used.\nThe duplicated names are: %s."),
                        paste(unique(names[dupl]), collapse = ", ")))
      }

      if (is.integer(data) || !is.numeric(data)) {
        # make sure that data is a matrix of numeric values
        data[] <- apply(data, MARGIN = c(1, 2), FUN = as.numeric)
      }

      return(data)
    }
  )
)
