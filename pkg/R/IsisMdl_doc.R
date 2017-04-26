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
#' the model variables
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
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{type}}{the variable type}
#' }
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
#' the equation names
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_eq_names(pattern = ".*", type =  c("all", "inactive"),
#'                  order =  order = c("sorted", "solve", "natural"))
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{type}}{the type (all or inactive)}
#' \item{\code{order}}{the ordering of the equations}
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
#'
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
#' \code{IsisMdl} method: @section Usage:
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
#' \item{\code{optons}}{a list with solve options}
#' \item{\code{fit_options}}{a list with options for the fit procedure}
#' } 
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$solve()
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
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} 
#' sets or update the rms values
#' @description
#' Sets or updates  the rms values
#' @section Usage:
#' \preformatted{
#' mdl$set_rms(values)
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object

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
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
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
