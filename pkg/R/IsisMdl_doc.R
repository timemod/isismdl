#' \code{\link{IsisMdl}} method: returns the names of the model variables
#' @name get_var_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the names of
#' the model variables
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_var_names(pattern = ".*", 
#'                   type =  c("all", "allfrml", "all_endolead"))
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{type}}{the variable type}
#' }
NULL

#' \code{\link{IsisMdl}} method: returns the the equation names
#' @name get_eq_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the
#' the equation names
#' @section Usage:
#' \preformatted{
#' mdl$get_eq_names(pattern = ".*", type =  c("all", "inactive"))
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{type}}{the type (all or inactive)}
#' }
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
#' mdl$set_eq_status(pattern, names, status =  c("active", "inactive"))
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{status}}{the equation states (all or inactive)}
#' }
#' 
#' 
#' If neither \code{pattern} nor \code{status} have been specified,
#' then all equations will be activated or deactivated.
NULL

#' \code{\link{IsisMdl}} method: Solves the model.
#' @name solve
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} solves
#' the model.
#' \code{IsisMdl} method: @section Usage:
#' \preformatted{
#' mdl$solve(period = mdl$get_period(), options = list(), 
#'           fit_options = list()
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{period}}{\code{\link[regts]{regperiod_range}
#'} object, or an object 
#' that can be coerced to \code{\link[regts]{regperiod_range}}}
#' \item{\code{optons}}{a list with solve options}
#' \item{\code{fit_options}}{a list with options for the fit procedure}
#' } 
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$solve()
NULL

#' \code{\link{IsisMdl}} method: Calculates missing model data from identities
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
#' mdl$fill_mdl_data(period = mdl$get_data_period())
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' 
#' @section Arguments:
#'
#' \describe{
#' \item{\code{period}}{a \code{\link[regts]{regperiod_range}} object}
#' }
#' @name fill_mdl_data
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$set_data(regts(NA, period = mdl$get_period()), names = "y")
#' mdl$fill_mdl_data()
NULL

#' \code{\link{IsisMdl}} methods: Retrieve timeseries from the model data, 
#' constant adjusments, fix values or fit targets
#' @name get_data
#' @aliases get_ca get_fix get_fit
#' @description
#' These methods of R6 class \code{\link{IsisMdl}} 
#' can be used to retrieve timeseries from the model data, 
#' constant adjusments, fix values or fit targets.
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_data(pattern, names, period = mdl$get_data_period())
#'
#' mdl$get_ca(pattern, names, period = mdl$get_period())
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
#' \item{\code{period}}{an \code{\link[regts]{regperiod_range}} object or an
#' object that can be coerced to a \code{regperiod_range}}
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
#' @name set_data
#' @aliases set_ca set_fix set_fit
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
#' @name set_values
#' @aliases set_ca_values set_fix_values set_fit_values
#' @description
#' These methods of R6 class \code{\link{IsisMdl}} 
#' can be used to set the values of the model data, constant adjusments,
#' fix values or fit targets.
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_values(value, names, pattern, period = mdl$get_data_period())
#'
#' mdl$set_ca_values(value, names, pattern, period = mdl$get_period())
#'
#' mdl$set_fix_values(value, names, pattern, period = mdl$get_period())
#'
#' mdl$set_fit_values(value, names, pattern, period = mdl$get_period())
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
#' \item{\code{period}}{an \code{\link[regts]{regperiod_range}} object or an
#' object that can be coerced to a \code{regperiod_range}}
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
#' @name change_data
#' @aliases change_ca
#' @description
#' This methods of R6 class \code{\link{IsisMdl}} 
#' changes the model data or constant adjustments by applying a function.
#'
#' @section Usage:
#' \preformatted{
#' mdl$change_data(fun, names, pattern, period = mdl$get_data_period())
#'
#' mdl$change_ca(fun names, pattern, period = mdl$get_period())
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
#' \item{\code{period}}{an \code{\link[regts]{regperiod_range}} object or an
#' object that can be coerced to a \code{regperiod_range}}
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
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} 
#' sets or update the rms values
#' @description
#' Sets or updates  the rms values
#' @section Usage:
#' \preformatted{
#' mdl$set_rms(values)
#' }

#' @section Arguments:
#'
#' \describe{
#' \item{\code{values}}{a named numeric vector with rms values}
#' }
NULL

#' \code{\link{IsisMdl}} method: Sets the solve options
#' @name set_solve_options
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} 
#' sets or update the rms values
#' @description
#' Sets or updates  the rms values
#' @section Usage:
#' \preformatted{
#' mdl$set_solve_options(...)
#' }

#' @section Arguments:
#'
#' \describe{
#' \item{\code{...}}{the solve options (TODO: document 
#' the possible options)}
#' }
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$set_solve_options(maxiter = 100)
NULL






