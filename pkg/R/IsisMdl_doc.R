#' \code{\link{IsisMdl}} method: returns the maximum lag of the model
#' @name get_maxlag
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the maximum
#' lag of the model
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_maxlag()
#'
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
NULL

#' \code{\link{IsisMdl}} method: returns the maximum lead of the model
#' @name get_maxlead
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the maximum
#' lead of the model
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_maxlead()
#'
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
NULL


#' \code{\link{IsisMdl}} method: returns the names of the model variables
#' @name get_var_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the names of
#' the model variables. By default, the function returns all variable
#' names. Argument \code{pattern} can be specified to select only
#' variables with names matching a regular expression. Argument \code{type}
#' can be specified to select variables with a specific type.
#' The following types are supported
#' \describe{
#' \item{\code{"allfrml"}}{all stochastic variables (both active and inactive).
#' Stochastic variables are variables that occur on the left hand side
#' of frml equations}
#' \item{\code{"all_endolead"}}{all variables with endogenous leads
#' (both active and inactive)}
#' \item{\code{"all"}}{all variables}
#' }
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_var_names(pattern = ".*",
#'                   type =  c("all", "allfrml", "all_endolead"))
#'
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression specifying variable names}
#' \item{\code{type}}{a character string specifying the variable type. See
#' the description above}
#' }
#' 
#' @examples
#' mdl <- islm_mdl()
#'
#' # get the names of all stochastic variables
#' mdl$get_var_names(type = "allfrml")
#'
#' # get all variables with names starting with "y":
#' mdl$get_var_names(pattern = "^y.*")
NULL

#' \code{\link{IsisMdl}} method: Sets labels for the model variables.
#' @name set_labels
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} sets labels
#' for the model variables.
#' @section Usage:
#' \preformatted{
#' mdl$set_labels(labels)
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{labels}}{a named character vector.
#' The names are the names of the model variables}
#' }
#' @examples
#' mdl <- islm_mdl()
#' mdl$set_labels(c(c = "Consumption", i = "investments"))
NULL

#' \code{\link{IsisMdl}} method: returns the names of the model variables
#' @name get_par_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the names of
#' the model parameters
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_par_names(pattern = ".*")
#'
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression}
#' }
NULL

#' \code{\link{IsisMdl}} method: returns the the equation names
#' @name get_eq_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the
#' the equation names.
#' Argument \code{pattern} can be specified to select only
#' equations with names matching a regular expression. Argument \code{type}
#' can be specified to select equations with a specific type.
#' The following types are supported
#' \describe{
#' \item{\code{"active"}}{active equations}
#' \item{\code{"inactive"}}{inactive equations}
#' \item{\code{"all"}}{all equations, the default}
#' }
#'
#' Argument \code{order} specifies the order of the eqautions returned.
#' The following ordering options are recognized:
#' \describe{
#' \item{\code{"sorted"}}{alphabetically ordering}
#' \item{\code{"solve"}}{ordered according to the solution sequence}
#' \item{\code{"natural"}}{same order as in the \code{mdl} file}
#' }
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_eq_names(pattern = ".*", type =  c("all", "active", "inactive"),
#'                  order =  c("sorted", "solve", "natural"))
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression specifying equation names}
#' \item{\code{type}}{the equation type, see description)}
#' \item{\code{order}}{the ordering of the equations (see description)}
#' }
#' @examples
#' mdl <- islm_mdl()
#'
#' # get the names of equations in solution order
#' mdl$get_eq_names(order = "solve")
#'
#' # get all equations with names starting with "y":
#' mdl$get_eq_names(pattern = "^y.*")
NULL

#' \code{\link{IsisMdl}} method: activates or de-activates one or more equations.
#' @name set_eq_status
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' can be used to set the equation status (active or inactive)
#' of one or more equations.
#'
#' This procedure is used to activate or deactivate a specified set
#' of equations.
#' After compiling a model, all equations are active.
#' Sometimes however it can be necessary to temporarily
#' exclude an equation from the model and
#' the solution process without actually removing it.
#'
#' Deactivating an equation implies that the left-hand side variable
#' becomes an exogenous variable. As long as an equation is
#' inactive, the corresponding left-hand side variable and any
#' constant adjustment (for \code{frml} equations) will remain
#' \emph{unchanged} in the model workspace.
#'
#' However the methods \code{set_data}, \code{set_ca},
#' \code{get_data} and \code{get_ca}
#' will still transfer data to and from the model workspace.
#'
#' A deactivated equation can also be reactivated. It will again
#' participate in the solution process and its left-hand side
#' variable will be treated as endogenous.
#'
#' Since deactivating effectively changes the structure of the model,
#' it may be necessary to compute a new ordering of the model.
#' This is not done automatically.
#' You must do it by hand. Currently, package \code{isismdl} does
#' not yet support reordering the model, but this feature will
#' become available in the future.
#'
#' If the left-hand side variable of a deactivated equation
#' appears as lead in the model, that lead will
#' temporarily be marked as an exogenous lead.
#' However, if a lead of another endogenous variable occurs
#' only in the deactivated equation that particular lead will \emph{not}
#' be registered as exogenous. The model will still be regarded as containing
#' endogenous leads and therefore the default solution mode will be
#' \code{ratex}, i.e. the Fair-Taylor method will be used for solving the model.
#'
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_eq_status(status =  c("active", "inactive"), pattern, names)
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{status}}{a character string specifying
#' the equation status (\code{"active"} or code{"inactive"})}
#' \item{\code{pattern}}{a regular expression specifying the names
#' of the equations}
#' \item{\code{names}}{a character vector with the names of the equations}
#' }
#'
#' If neither \code{pattern} nor \code{status} have been specified,
#' then all equations will be activated or deactivated.
#' @seealso \code{\link{get_eq_names}}
#' @examples
#' mdl <- islm_mdl()
#'
#' # deactivate equation "c" and "i"
#' mdl$set_eq_status("inactive", names = c("c", "i"))
#' 
#' # deactivate all equations starting with "y" ("y" and "yd")
#' mdl$set_eq_status("inactive", pattern = "^y*")
#'
#' # print all deactivated equations
#' print(mdl$get_eq_names(type = "inactive"))
NULL

#' \code{\link{IsisMdl}} method: sets the model period
#' @name set_period
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} sets the model period.
#' This is the default period used when solving the model.
#'
#' If the model data has not already been initialized with method
#' \code{\link{init_data}}, then \code{set_period} also initializes
#' the model data. In that case the model data period is set to the
#' specified model period extended with a lag and lead period.
#' Model timeseries are initialized with \code{NA} and all constant
#' adjustments with 0.
#'
#' If the model data has already been initialized with  method
#' \code{\link{init_data}}, then the new model period
#' should be compatible with the model data period.
#' In particular, the new model period extended with a lag and lead period
#' should not contain periods outside the model data period.
#' @section Usage:
#' \preformatted{
#' mdl$set_period(period)
#'
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{period}}{\code{\link[regts]{period_range}}
#' object, or an object that can be coerced to
#' \code{\link[regts]{period_range}}}
#' }
#'
#' @examples
#' mdl <- islm_mdl()
#' mdl$set_period("2017Q2/2021Q3")
NULL

#' \code{\link{IsisMdl}} method: initialized the model data.
#' @name init_data
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} initializes the
#' model data.
#'
#' This method sets the model data period and initializes
#'  the model variables and constant adjustmemts.
#'
#' You have to specify one of the two arguments \code{data_period}
#' and \code{data}. If \code{data_period}  has not been specified,
#'  then the model data period is set to the period range of
#' \code{data}. If \code{data} has not been specified,
#' then argument \code{data_period} is mandatory.
#'
#' The method first initializes all model timeseries with \code{NA}
#' and all constant adjustments with 0 for the data period.
#' If arguments \code{data} or \code{ca} have been specified,
#'  then the model variables or constant adjustments are
#' subsequently updated with the timeseries \code{data} or \code{ca},
#' respectively.
#'
#' This methods also sets the model period, the standard period
#' for which the model will be solved. The model period
#' is obtained from the data period by subtracting the lag and lead periods.
#' @section Usage:
#' \preformatted{
#' mdl$init_data(data_period, data, ca)
#'
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{data_period}}{\code{\link[regts]{period_range}}
#' object, or an object that can be coerced to
#' \code{\link[regts]{period_range}}}
#' \item{\code{data}}{a \code{\link[stats]{ts}} or \code{\link[regts]{regts}}
#'  object}
#' }
NULL

#' \code{\link{IsisMdl}} method: returns the model period
#' @name get_period
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the model period.
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_period()
#'
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
#' @seealso
#' \code{\link{set_period}}
NULL

#' \code{\link{IsisMdl}} method: returns the model data period
#' @name get_data_period
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the model data period.
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_data_period()
#'
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
#' @seealso
#' \code{\link{set_period}}
NULL

#' \code{\link{IsisMdl}} method: Solves the model.
#' @name solve
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} solves
#' the model.
#' @section Usage:
#' \code{IsisMdl} method: 
#' \preformatted{
#' mdl$solve(period = mdl$get_period(), options = list(),
#'           fit_options = list()
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{period}}{\code{\link[regts]{period_range}}
#' object, or an object
#' that can be coerced to \code{\link[regts]{period_range}}}
#' \item{\code{options}}{a named list with solve options,
#' for example \code{list(maxiter = 50)}. 
#' The names are the corresponding argument names of 
#' method \code{\link{set_solve_options}}
#' The specified options will only used in this call of
#' {solve()} and will not be stored in the \code{IsisMdl} object}
#' \item{\code{fit_options}}{a named list with options for the fit procedure,
#' for example \code{list(maxiter = 10)}. 
#' The names are the corresponding argument names of 
#' method \code{\link{set_fit_options}}
#' The specified options will only used in this call of
#' {solve()} and will not be stored in the \code{IsisMdl} object}
#' }
#' @seealso \code{\link{set_solve_options}} and 
#' \code{\link{set_fit_options}}
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$solve(options = list(report = "fullrep"))
NULL

#' \code{\link{IsisMdl}} method: Calculates missing model data from identities
#' @name fill_mdl_data
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' attempts to calculate missing data for endogenous
#' variables of a model by solving the identity equations in solution order.
#'
#' The procedure can be used to fill in data before and beyond the
#' model period (as set by method \code{set_period} for as many
#' variables as possible.
#'
#' @section Usage:
#' \preformatted{
#' mdl$fill_mdl_data(period = mdl$get_data_period(),
#'                   report = c("period", "minimal", "no"))
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{period}}{a \code{\link[regts]{period_range}} object}
#' \item{\code{report}}{Defines the type of report about the number of
#' replaced missing values. See details.}
#' }
#'
#' @section Details:
#'
#' Argument \code{report} can be used to specify
#' the type of report about the number of replaced missing values
#' Specify
#' \describe{
#' \item{\code{minimal}}{to get a minimal report. Only the total number
#' of replaced missing values is reported}
#' \item{\code{period}}{to get a report per period (default). For each period
#' the number of replaced missing values is reported}
#' \item{\code{no}}{to not generate a report}
#' }
#'
#'
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$set_data(regts(NA, period = mdl$get_period()), names = "y")
#' mdl$fill_mdl_data()
NULL

#' \code{\link{IsisMdl}} method: runs model equations
#' @name run_eqn
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' runs specific equations of the model separately.
#' Each specified equation is run separately for the specified period.
#' If the equation is a stochastic equation (a \code{frml} equation)
#' and the corresponding endogenous variable has been fixed
#' then the constant adjustment for the equation will be calculated
#' such that the result of the equation equals the predetermined required
#' value for the left-hand side.
#'
#' If neither argument \code{pattern} or \code{names} have been specified,
#' then all active model equations are ran in solve order.
#'
#' @section Usage:
#' \preformatted{
#' mdl$run_eqn(pattern, names, period = mdl$get_data_period())
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression. Equations with names
#' matching the regular expression are run in solve order}
#' \item{\code{names}}{a character vector with equation names. The
#' corresponding equations are solved in the order as they are
#' specified}
#' \item{\code{period}}{a \code{\link[regts]{period_range}} object}
#' }
#'
#' @examples
#' mdl <- islm_mdl("2017Q1/2019Q3")
#' mdl$run_eqn(names = c("c", "t"))
NULL


#' \code{\link{IsisMdl}} methods: Retrieve timeseries from the model data,
#' constant adjusments, fix values or fit targets
#' @name get_data-methods
#' @aliases get_data get_ca get_fix get_fit
#' @description
#' These methods of R6 class \code{\link{IsisMdl}}
#' can be used to retrieve timeseries from the model data,
#' constant adjusments, fix values or fit targets.
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_data(pattern, names, period = mdl$get_data_period())
#'
#' mdl$get_ca(pattern, names, period = mdl$get_data_period())
#'
#' mdl$get_fix()
#'
#' mdl$get_fit()
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{names}}{a character vector with variable names}
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{period}}{an \code{\link[regts]{period_range}} object or an
#' object that can be coerced to a \code{period_range}}
#' }
#' @section Methods:
#' \itemize{
#' \item \code{get_mdl_data}: Model data
#'
#'\item \code{get_ca}: Constant adjustments
#'
#'\item \code{get_fix_values}: Fix values
#'
#'\item \code{get_fit_targets}: Fit targets
#'}
NULL

#' \code{\link{IsisMdl}} methods: transfers data from a timeseries
#' object to the model data, constant adjusments, fix values or fit targets.
#' @name set_data-methods
#' @aliases set_data set_ca set_fix set_fit
#' @description
#' These methods of R6 class \code{\link{IsisMdl}}
#' Transfers data from a timeseries object to the model data,
#' constant adjusments, fix values or fit targets.
#'
#' If \code{data} has labels, then method \code{set_data} will update
#' the labels of the corresponding model variables
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_data(data, names = colnames(data))
#'
#' mdl$set_ca(data, names = colnames(data))
#'
#' mdl$set_fix(data, names = colnames(data))
#'
#' mdl$set_fit(data, names = colnames(data))
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{data}}{a \code{\link[stats]{ts}} or \code{\link[regts]{regts}}
#'  object}
#' \item{\code{names}}{a character vector with variable names. Defaults to the
#' column names of \code{data}. If \code{data} does not have column names,
#' then argument \code{names} is mandatory}
#' }

#' @section Methods:
#'
#' \itemize{
#' \item \code{set_data}: Set model data
#'
#'\item \code{set_ca}: Set constant adjustments
#'
#'\item \code{set_fix}: Set fix values
#'
#'\item \code{set_fit}: Set fit values
#'}
NULL

#' \code{\link{IsisMdl}} methods: Sets the values of the model data,
#' constant adjusments, fix values or fit targets
#' @name set_values-methods
#' @aliases set_values set_ca_values set_fix_values set_fit_values
#' @description
#' These methods of R6 class \code{\link{IsisMdl}}
#' can be used to set the values of the model data, constant adjusments,
#' fix values or fit targets.
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_values(value, names, pattern, period = mdl$get_data_period())
#'
#' mdl$set_ca_values(value, names, pattern, period = mdl$get_data_period())
#'
#' mdl$set_fix_values(value, names, pattern, period = mdl$get_data_period())
#'
#' mdl$set_fit_values(value, names, pattern, period = mdl$get_data_period())
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{value}}{a numeric vector of length 1 or with the same length
#' as the length of the range of \code{period}}
#' \item{\code{names}}{a character vector with variable names}
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{period}}{an \code{\link[regts]{period_range}} object or an
#' object that can be coerced to a \code{period_range}}
#' }
#' @section Methods:
#' \itemize{
#' \item \code{set_values}: Model data
#'
#'\item \code{set_ca_values}: Constant adjustments
#'
#'\item \code{set_fix_values}: Fix values
#'
#'\item \code{set_fit_values}: Fit targets
#'}
NULL

#' \code{\link{IsisMdl}} methods: changes the model data or constant adjustments by applying a function.
#' @name change_data-methods
#' @aliases change_data change_ca
#' @description
#' This methods of R6 class \code{\link{IsisMdl}}
#' changes the model data or constant adjustments by applying a function.
#'
#' @section Usage:
#' \preformatted{
#' mdl$change_data(fun, names, pattern, period = mdl$get_data_period())
#'
#' mdl$change_ca(fun names, pattern, period = mdl$get_data_period())
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{fun}}{a function}
#' \item{\code{names}}{a character vector with variable names}
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{period}}{an \code{\link[regts]{period_range}} object or an
#' object that can be coerced to a \code{period_range}}
#' \item{\code{...}}{arguments passed to \code{fun}}
#' }
#' @section Methods:
#' \itemize{
#' \item \code{change_values}: Model data
#'
#'\item \code{change_ca}: Constant adjustments
#'}
NULL

#' \code{\link{IsisMdl}} method: Sets or updates  the rms values
#' @name set_rms
#' @aliases get_rms
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' sets or update the root mean square error data 
#' used in the fit procedure. Root mean square errors
#' equal to 0 or \code{NA} are not used in the fit procedure.
#'
#' Method \code{get_rms} returns all non-\code{NA} rms values,
#'
#' @description
#' Sets or updates  the rms values
#' @section Usage:
#' \preformatted{
#' mdl$set_rms(values)
#'
#' mdl$get_rms()
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object

#' @section Arguments:
#'
#' \describe{
#' \item{\code{values}}{a named numeric vector with rms values}
#' }
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$set_rms(c(c = 5.0, t = 2, i = 21, md = 2))
#' print(mdl$get_rms())
NULL

#' \code{\link{IsisMdl}} method: Sets the solve options
#' @name set_solve_options
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} can be used to set one or more
#' solve options. These options will be stored in the \code{IsisMdl} object.
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_solve_options(mode, fbstart, maxiter, maxjacupd, rlxspeed,
#'                       rlxmin, rlxmax, cstpbk, cnmtrx, xrelax,
#'                       xmaxiter, xupdate, dbgopt, erropt,
#'                       report, ratreport, ratreport_rep, ratfullreport_rep,
#'                       bktmax, xtfac)
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#' 
#' All arguments below expect a numerical value unless mentioned otherwise.
#'
#' \describe{
#' \item{\code{mode}}{a character string specifying the solution mode
#' (\code{"auto"}, \code{"ratex"}, \code{"dynamic"}, \code{"reschk"},
#' \code{"backward"} or \code{"static"}). \code{"auto"} is the default.
#' See section "Solution modes" below}
#' \item{\code{fbstart}}{a character string specifying
#' the method of initialising feedback values.
#' (\code{"current"}, \code{"previous"}, \code{"curifok"} or \code{"previfok"}).
#' The default is \code{"current"}. See section "Feedback initialisation
#' methods" below}
#' \item{\code{maxiter}}{the maximum number of iterations per period (default 50)}
#' \item{\code{maxjacupd}}{the maximum number of Newton Jacobian updates per period
#'  (default 10)}
#'  \item{\code{rlxspeed}}{Newton relaxation shrinkage (default is 0.5)}
#'  \item{\code{rlxmin}}{Minimum Newton relaxation factor (default is 0.05)}
#'  \item{\code{rlxmax}}{Maximum Newton relaxation factor (default is 1.0)}
#'  \item{\code{cstpbk}}{Stepback criterion (default is 1.3).
#' If the convergence criterium \code{Fcrit} is larger
#' than \code{cstpbk} or invalid feedback variables 
#' have been obtained then the Newton step
#' is not accepted and linesearching will be initiated. 
#' If the linesearching procedure failed
#' (\code{Fcrit} is still larger than \code{cstpbk}
#' after the maximum number of linesearch steps \code{bktmax}
#' has been reached or if the relaxation
#' factor has become smaller than \code{rlxmin}), 
#' a new Jacobian matrix is computed.  
#' In each linesearch step the current relaxation factor is shrunk by 
#' \code{rspeed}. 
#' The relaxation factor is set to its maximum value
#' \code{rlxmax}) when a new Jacobian has been calculated.
#' }
#'  \item{\code{cnmtrx}}{Recalculate matrix criterion (default is 0.9).
#' If the convergence criterium \code{Fcrit} is larger
#' than \code{cnmtrx} but smaller than \code{cstpbk},
#' the Newton step is accepted but a new Jacobian is computed
#' and the relaxation factor is set to its maximum value
#' \code{rlxmax}.
#' The new Jacobian is used in the next step. However, the 
#' Jacobian will not be recalculated if the number of Jacobian updates
#' in a period is larger than \code{maxjacupd}}
#'  \item{\code{xrelax}}{Rational expectations relaxation factor (default is 1)}
#'  \item{\code{xmaxiter}}{Maximum number of rational expectation iterations
#' (default is 10)}
#'  \item{\code{xupdate}}{Character string defining the method of updating
#' leads. Possible values are \code{"fixed"} (the default) and \code{"lastval"}.
#' For \code{"fixed"} the leads beyond the solution period
#' are fixed at the initial values. For \code{"lastval"} leads beyond 
#' the solution period take on the values from the last solution date}
#' \item{\code{dbgopt}}{A character vector specifying one more
#' debugging options. See section "Debugging options" below}
#' \item{\code{erropt}}{Character string defining the error handling when 
#' invalid lags, leads and/or exogenous variables are detected.
#' Possible values are \code{"stop"} (stop on errors) 
#' and \code{cont} (continue on errors). The default is \code{"stop"}}
#' \item{\code{report}}{A character string defining the type of 
#' computation progress report. Possible values are
#' \code{"period"} (for a report per period),
#' \code{"fullrep"} (for a full report), 
#' \code{"minimal"}  (for a minimal report), and
#' \code{"no"}  (for no report). The default is \code{"period"}}
#' \item{\code{ratreport}}{Defines the type of rational expectations progress
#' report. See section "Ratex report options" below}
#' \item{\code{ratreport_rep}}{An integer number specifying
#' the Fair-Taylor report repetition count.
#' See Section "Ratex report options" below}
#' \item{\code{ratfullreport_rep}}{An integer number, specifying
#' the Fair-Taylor full report repetition count. See Section 
#' "Ratex report options" below}
#' \item{\code{bktmax}}{Maximum number of backtracking linesearch steps 
#' with old jacobian. Sometimes it is necessary for the Broyden 
#' method to take a shorter step than the standard step. This is called
#' backtracking linesearch. \code{bktmax} is the maximum number of
#' line search steps before a new Jacobian is computed}
#' \item{\code{xtfac}}{Rational expectations convergence test multiplier
#' When using the \code{"ratex"} solution mode,
#' convergence of endogenous leads cannot be tested to the accuracy used in
#' testing for convergence in the solution of the model.
#' This option specifies the multiplier to apply to the convergence criterion
#' for each endogenous variable if the variable has an endogenous lead.
#' Suppose for example that some variable has a convergence criterion of $10^{-5}$
#' and assume a value of 10 for the multiplier.
#' Then its endogenous lead will be regarded as converged}
#' }
#'
#' @section Solution modes:
#'
#' The solution mode can be specified with argument \code{mode}.
#' Possible values are:
#' \describe{
#' \item{\code{"auto"}}{determine the solution mode automatically:
#' \code{"ratex"} for models with endogenous leads
#' and \code{"dynamic"} for models without endogenous leads}
#' \item{\code{"dynamic"}}{to update lags and current values of all
#' right-hand side endogenous variables (leads are not updated). This is the
#' default for models without endogenous leads}
#' \item{\code{"ratex"}}{to update lags, leads and current values of all
#' right-hand side endogenous variables. This is the default mode
#' for models with endogenous leads.
#' The model is solved in dynamic mode for all periods conditional
#' on the endogenous right-hand side leads. After solving for the complete
#' solution period the endogenous leads are updated with the the results
#' for the corresponding endogenous variables. The solution process thus consists
#' of an two loops: an inner loop solving the model  for all periods given the leads and
#' an outer loop which solves for the endogenous leads}
#' \item{\code{"static"}}{to update only current values of right-hand side
#' endogenous variables lags and leads are not modified}
#' \item{\code{"reschk"}}{to not update right-hand side
#' endogenous variables from the solution}
#' \item{\code{"backward"}}{same as dynamic, except that the model is solved
#' backwards. The model is solved in reversed order by starting at the last
#' solution period and ending at the first solution period. Leads are updated
#' and lags are not updated}
#' }
#' If a model contains leads then the \code{"ratex"} mode is the
#' default; this is a Fair-Taylor algorithm.
#'
#' The default is \code{"auto"}.
#'
#' @section Feedback initialisation methods:
#' 
#' Argument \code{fbstart} can be used to specify 
#' the way how the feedback variables at the current period
#' (i.e. the period for which the model is being solved)
#' are initialised from the model data.
#' Possible values of \code{fbstart} are:
#' \describe{
#' \item{\code{"current"}}{the initial values are always taken from the
#' current period. This is the default}
#' \item{\code{"previous"}}{The initial values are
#' taken from the previous period except when the first period
#' to be solved is the start of the model data period.
#' In that case current period values are used}
#' \item{\code{"curifok"}}{Current period
#' values are used if they are valid otherwise previous period
#' values are used}
#' \item{\code{"previfok"}}{At the start of the solution
#' period, previous period values will be used if they are available
#' and valid; otherwise current period values will be used.
#' Thereafter previous period initial values are always used, which is
#' equivalent to the \code{"previous"} method}
#' }
#' The default is \code{"current"}.
#'
#' @section Debugging options:
#'
#' Argument \code{dbgopt} can be used to specify one or more
#' options for debugging.  Possible values are
#' \describe{
#' \item{\code{"prifb"}}{print feedback variables at each iteration}
#' \item{\code{"prild"}}{print all leads at each ratex iteration}
#' \item{\code{"prijac"}}{print jacobian matrix when updated}
#' \item{\code{"prinoconv"}}{print all not converged endogenous variables}
#' \item{\code{"prinotconvl"}}{print all not converged leads}
#' \item{\code{"allinfo"}}{all of the above}
#' \item{\code{"noprifb"}}{do not print feedback variables at each iteration}
#' \item{\code{"noprild"}}{do not print all leads at each ratex iteration}
#' \item{\code{"noprijac"}}{do not print jacobian matrix when updated}
#' \item{\code{"noprinoconv"}}{print only the largest discrepancy of
#' all not converged endogenous variables}
#' \item{\code{"noprinotconvl"}}{print only the largest discrepancy of
#' all not converged leads}
#' \item{\code{"noinfo"}}{no debugging output}
#' \item{\code{"priscal"}}{print scaling factors as determined from the
#' jacobian}
#' \item{\code{nopriscal}}{do not print scaling factors as determined
#' from the jacobian}
#' }
#' Default is no printing of debugging information.

#' @section Ratex report options:
#'
#' The type of report is determined by argument \code{ratreport}.
#' Arguments \code{ratreport_rep} (the report repetion count)
#' and \code{ratfullreport_rep} (the full report repetition count),
#' both specified as integer numbers,
#' can be used to futher modify the progress report.
#'
#' Possible values for \code{ratreport} are
#' \describe{
#' \item{\code{"iter"}}{print the number of not converged 
#' expectation values every \code{ratreport_rep} Fair Taylor iteration
#' (the default)}
#' \item{\code{"fullrep"}}{full report. The number of not converged 
#' expectation values is printed every \code{ratreport_rep} Fair Taylor
#'  iteration and the largest remaining discrepancy every 
#' \code{ratfullreport_rep} Fair Taylor iteration}
#' \item{\code{"minimal"}}{for a full report only after the last Fair Taylor
#' iteration}
#' }
#' 
#' If \code{ratfullreport_rep} is \code{NA}, then the full report
#' is printed every \code{ratreport_rep} Fair-Taylor iteration.
#' The default values for \code{ratreport_rep}  and \code{ratfullreport_rep}
#' are 1 and \code{NA}, respectively.
#'
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$set_solve_options(maxiter = 100)
NULL

#' \code{\link{IsisMdl}} method: Sets the options for the fit procedure.
#' @name set_fit_options
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} can be used to set one or more
#' options for the fit procedure. 
#' These options will be stored in the \code{IsisMdl} object.
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_fit_options(maxiter, cvgabs, mkdcrt, report, dbgopt)
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#' 
#' All arguments below expect a numerical value unless mentioned otherwise.
#'
#' \describe{
#' \item{\code{maxiter}}{The maximum number of iterations (default 5)}
#' \item{\code{cvgabs}}{Criterion for absolute convergence.
#' When the largest scaled discrepancy of the fit target values is
#' less than \code{cvgabs}, the fit procedure has converged.
#' The default value is 100 times the square root of
#' the machine precision (\code{100 * sqrt(.Machine$double.eps)}),
#' which is typically \code{1.5e-6}.}
#' \item{\code{mkdcrt}}{Criterion for calculating a new fit jacobian.
#' When the ratio of two successive largest scaled discrepancies of
#' the fit target values is
#' larger than \code{mkdcrt} a new fit jacobian will be calculated
#' in the next iteration. Any value specified must lie between 0.05
#' and 0.95. The default value is 0.5.}
#' \item{\code{report}}{A character string specifying the the
#' type of report of the fit procedure for each period.
#' Possible values are \code{"fullrep"} 
#' (the default, an iteration report is printed for each period)
#' and  \code{"minimal"} (for a one line summary).}
#' \item{\code{dbgopt}}{A character vector specifying one or more
#' debug options. See section "Debugging options" below}
#' }
#' 
#' @section Debugging options:
#' 
#' Argument \code{dbgopt} can be used to specify one or more
#' options for debugging the fit procedure.  Possible values are
#' \describe{
#' \item{\code{prica}}{print the fit jacobian every time it is calculated}
#' \item{\code{noprica}}{do not print the fit jacobian every time it is
#' calculated}
#' \item{\code{prijac}}{print the fit jacobian every time it is calculated}
#' \item{\code{noprijac}}{do not print the fit jacobian every time it is
#' calculated}
#' \item{\code{supsot}}{to suppress all output of the normal solution process}
#' \item{\code{nosupsot}}{to not suppress all output of the normal solution
#' process. Output will be a mess if this option is used}
#' }
#' The default debug options are \code{c("noprica", "noprijac", "supsot")}
NULL

#'
#' \code{\link{IsisMdl}} method: Returns the labels of the model variables.
#' @name get_labels
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' returns the labels of the model variables.
#' @section Usage:
#' \preformatted{
#' mdl$get_labels()
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @seealso
#' \code{\link{set_labels}}
NULL

#' \code{\link{IsisMdl}} method: Sets the convergence criterion for selected
#' variables.
#' @name set_cvgcrit
#' @aliases get_cvgcrit
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} sets the
#' convergence criterion for one or more endogenous model variables.
#' A variable \eqn{x} has converged when two
#' successive values \eqn{x_2} and
#' \eqn{x_1} satisfy the following
#' condition 
#' \deqn{|x_2 - x_1| \le \epsilon \max(1,|x_1|)}
#' where \eqn{\epsilon} is
#' the convergence criterion for the tested
#' variable.
#' 
#' The default value of \eqn{\epsilon} for all variables
#' is the square root of the machine precision
#' (\code{sqrt(.Machine$double.eps)}, typically about \code{1.5e-8})
#' 
#' Method \code{get_cvgcrit()} returns
#' the convergence criteria for all model variables
#' @section Usage:
#' \preformatted{
#' mdl$set_cvgcrit(value, pattern, names) 
#'
#' mdl$get_cvgcrit()
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{value}}{convergence criterion. This must be 
#' a small positive number}
#' \item{\code{pattern}}{a regular expression specifying the
#' variable names}
#' \item{\code{names}}{a character vector with variable names}
#' }
#'
#' If neither \code{pattern} nor \code{names} have been specified,
#' then the convergence criterion of all endogenous variables
#' will be set to the specified value.
#'
#' @examples
#' mdl <- islm_mdl()
#'
#' # set convergence criterion for variables "c" and "i":
#' mdl$set_cvgcrit(1e-4, names = c("c", "i"))
#' 
#' # set convergence criterion for variables "y" and "yd":
#' mdl$set_cvgcrit(1e-4, pattern = "^y*")
#'
#' print(mdl$get_cvgcrit())
NULL

#' \code{\link{IsisMdl}} method: Sets the Fair-Taylor relaxtion factors
#' @name set_ftrelax
#' @aliases get_ftrelax
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} sets the
#' Fair-Taylor relaxtion factors for the endogenous leads.
#' 
#' Method \code{get_ftrelax()} returns
#' the Fair-Taylor relaxtion factors
#' for all endogenous leads.
#' @section Usage:
#' \preformatted{
#' mdl$set_ftrelaxvalue, pattern, names) 
#'
#' mdl$get_ftrelax()
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{value}}{Fair-Taylor relaxtion number.
#' This must be a positive number or \code{NA} to disable any previously set value.
#' The default value for all endogenous leads is \code{NA}, which means that
#' the general uniform Fair-taylor relaxation 
#' (solve option \code{ftrelax}, see \code{\link{set_solve_options}})
#' will be applied}
#' \item{\code{pattern}}{a regular expression specifying the
#' variable names}
#' \item{\code{names}}{a character vector with variable names}
#' }
#'
#' If neither \code{pattern} nor \code{names} have been specified,
#' then the Fair-Taylor relaxtion factors of all variables
#' with endogenous leads will be set to the specified values.
#'
#' @examples
#' mdl <- ifn_mdl()
#'
#' # set Fair-relaxtion factor all all variables with names of length 2 
#' # to 0.8:
#' mdl$set_ftrelax(0.5, pattern = "^..$")
#'
#' # set Fair-relaxtion factor for variable "lambda":
#' mdl$set_ftrelax(0.5, names = "lambda")
#' 
#' print(mdl$get_ftrelax())
NULL


#' \code{\link{IsisMdl}} method: Sets the model parameters
#' @name set_param
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' sets the model parameters
#' @section Usage:
#' \preformatted{
#' mdl$set_param(p)
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @section Arguments:
#'
#' \describe{
#' \item{\code{p}}{a named list.
#' The names are the names of the parameter names.
#' The list elements are numeric vectors with a length equal to the length
#' of the corresponding parameter}
#' }
#' @examples
#' mdl <- islm_mdl()
#' mdl$set_param(list(i0 = 101))
NULL


#' \code{\link{IsisMdl}} method: Returns model parameters
#' @name get_param
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' returns model parameters
#' @section Usage:
#' \preformatted{
#' mdl$get_param(pattern, names)
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{names}}{a character vector with parameter names}
#' }
#' @seealso
#' \code{\link{set_param}}
NULL

#' Writes the model to an RDS file
#' @name write_mdl
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' serializes the model object and writes it
#' to an RDS file. The model can be read back by function
#' \code{\link{read_mdl}}.
#' @section Arguments:
#'
#' \describe{
#' \item{\code{file}}{the name of the RDS file}
#' }
#' @examples
#' mdl <- islm_mdl("2017Q1/2019Q2")
#' mdl$write_mdl("islm_mdl.rds")
#' @seealso \code{\link{read_mdl}}
NULL
