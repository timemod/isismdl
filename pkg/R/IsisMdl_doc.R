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


#' \code{\link{IsisMdl}} method: returns the names of the endogenous model
#' variables
#' @name get_endo_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the names of
#' the endogenous model variables. By default, the function returns the
#' names of all active endogenous variables. Argument \code{pattern} can be
#' specified to select only variables with names matching a regular expression.
#' Argument \code{type}
#' can be specified to select variables with a specific type.
#' The following types are supported
#' \describe{
#' \item{\code{"frml"}}{stochastic variables.
#' Stochastic variables are variables that occur on the left hand side
#' of frml equations}
#' \item{\code{"lags"}}{all endogenous variables with lags}
#' \item{\code{"leads"}}{all endogenous variables with leads}
#' \item{\code{"feedback"}}{all feedback variables}
#' \item{\code{"all"}}{all endogenous variables, the default}
#' }
#'
#' If some equation have been deactivated (see \code{\link{set_eq_status}}),
#' then argument \code{status} may be useful.
#' By default, the function only returns the names of the active
#' endogenous variables, i.e. the variables that occur on the
#' left hand side of active equation. This behaviour can be modified
#' by specifying \code{status}. The following options
#' for argument \code{status} are recognized:
#'\describe{
#' \item{\code{"active"}}{active endogenous variables, the default}
#' \item{\code{"inactive"}}{inactive endogenous variables}
#' \item{\code{"all"}}{all endogenous variables}
#' }
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_endo_names(pattern = ".*",
#'                   type =  c("all", "frml", "lags", "leads", "feedback"),
#'                   status = c("active", "inactive", "all"))
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
#' \item{\code{status}}{a character string specifying the status of the
#' endogenous variable (inactive or active). See the description above}
#' }
#'
#' @seealso \code{\link{get_exo_names}} and \code{\link{get_var_names}}
#' @examples
#' mdl <- islm_mdl()
#'
#' # get the names of all stochastic variables
#' mdl$get_endo_names(type = "frml")
#'
#' # get all variables with names starting with "y":
#' mdl$get_endo_names(pattern = "^y.*")
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

#' \code{\link{IsisMdl}} method: returns the names of the exogenous model
#' variables
#' @name get_exo_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the names of
#' the exogenous model variables, including
#' the left hand side variables of inactive equations
#' (see \code{\link{set_eq_status}}).
#' @section Usage:
#' \preformatted{
#' mdl$get_exo_names(pattern = ".*")
#' }
#'
#' \code{mdl} is an \code{IsisMdl} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression specifying variable names}
#' }
#' @seealso \code{\link{get_endo_names}} and \code{\link{get_var_names}}
#' @examples
#' mdl <- islm_mdl()
#'
#' # get the names of all exogenous model variables
#' mdl$get_exo_names()
#'
#' # get all variables with names starting with "g":
#' mdl$get_exo_names(pattern = "^g.*")
NULL

#' \code{\link{IsisMdl}} method: returns the names of the model
#' variables
#' @name get_var_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the names of
#' the model variables, both exogenous and endogenous.
#'
#' Argument \code{type} can be specified to select variables with a specific type.
#' The following types are supported
#' \describe{
#' \item{\code{"all"}}{All model variables}
#' \item{\code{"lags"}}{Model variables with lags}
#' \item{\code{"leads"}}{Model variables with leads}
#' }
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_var_names(pattern = ".*", type = c("all", "lags", "leads"))
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

#'
#' @seealso \code{\link{get_endo_names}} and \code{\link{get_exo_names}}
#' @examples
#' mdl <- islm_mdl()
#'
#' # get the names of all model variables
#' mdl$get_var_names()
#'
#' # get all variables with names starting with "y":
#' mdl$get_var_names(pattern = "^y")
#'
#' # get the names of all lagged variables
#' mdl$get_var_names(type = "lags")
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
#' \item{\code{pattern}}{a regular expression specifying parameter names}
#' }
#' @examples
#' mdl <- islm_mdl()
#'
#' # print all model parameter names
#' print(mdl$get_par_names())
#'
#' # print names of model parameters with names starting with c
#' print(mdl$get_par_names(pattern = "^c.*"))
NULL

#' \code{\link{IsisMdl}} method: returns the the equation names
#' @name get_eq_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the
#' the equation names.
#' Argument \code{pattern} can be specified to select only
#' equations with names matching a regular expression. Argument \code{status}
#' can be specified to select only the active or inactive equations.
#' Possible options for argument \code{status are}
#' \describe{
#' \item{\code{"active"}}{active equations}
#' \item{\code{"inactive"}}{inactive equations}
#' \item{\code{"all"}}{all equations, the default}
#' }
#'
#' Argument \code{order} specifies the order of the equations returned.
#' The following ordering options are recognized:
#' \describe{
#' \item{\code{"sorted"}}{alphabetically ordering}
#' \item{\code{"solve"}}{ordered according to the solution sequence}
#' \item{\code{"natural"}}{same order as in the \code{mdl} file}
#' }
#'
#' @section Usage:
#' \preformatted{
#' mdl$get_eq_names(pattern = ".*", status =  c("all", "active", "inactive"),
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
#' \item{\code{status}}{the equation status, see Description}
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
#' This is not done automatically. Use method \code{\link{order}}.
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
#' the equation status (\code{"active"} or \code{"inactive"})}
#' \item{\code{pattern}}{a regular expression specifying the names
#' of the equations}
#' \item{\code{names}}{a character vector with the names of the equations}
#' }
#'
#' If neither \code{pattern} nor \code{status} have been specified,
#' then all equations will be activated or deactivated.
#' @seealso \code{\link{get_eq_names}} and \code{\link{order}}
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
#' print(mdl$get_eq_names(status = "inactive"))
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

#' \code{\link{IsisMdl}} method: initializes the model data.
#' @name init_data
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' initializes the model variables and constant adjustments for the whole
#' data period.
#' The model timeseries are set to \code{NA} and the constant adjustments to
#' zero.
#'
#' If arguments \code{data} or \code{ca} have been specified,
#' then the model variables or constant adjustments are
#' subsequently updated with the timeseries in \code{data} or \code{ca},
#' respectively. Timeseries in `data` or `ca` that are no model variables
#' or constant adjustments are silently skipped.
#'
#' If the model period has not yet been specified (in function
#' \code{\link{isis_mdl}} or method \code{\link{set_period}}), then
#' this method also sets the model period, the standard period
#' for which the model will be solved. The model period
#' is obtained from the data period by subtracting the lag and lead periods.
#'
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
#' \item{\code{data_period}}{a \code{\link[regts]{period_range}}
#' object, or an object that can be coerced to
#' \code{\link[regts]{period_range}}.  If not specified then the data period
#' is based on the period range of argument \code{data} (if this argument
#' has been specified) and  the model period.}
#' \item{\code{data}}{a \code{\link[stats]{ts}} or \code{\link[regts]{regts}}
#'  object with model variables}
#' \item{\code{ca}}{a \code{\link[stats]{ts}} or \code{\link[regts]{regts}}
#'  object with constant adjustments}
#' }
#' @seealso \code{\link{set_period}}
#' @examples
#' mdl <- islm_mdl()
#' mdl$init_data("2017Q2/2021Q3")
#' print(mdl)
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
#' the model. It requires that the model period has been
#' set with methods \code{\link{isis_mdl}}, \code{\link{init_data}}
#' or \code{\link{set_period}}).

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
#' \code{solve()} and will not be stored in the \code{IsisMdl} object}
#' \item{\code{fit_options}}{a named list with options for the fit procedure,
#' for example \code{list(maxiter = 10)}.
#' The names are the corresponding argument names of
#' method \code{\link{set_fit_options}}
#' The specified options will only used in this call of
#' \code{solve()} and will not be stored in the \code{IsisMdl} object}
#' }
#'
#' @section Details:
#'
#' The model will be solved for each subperiod from the
#' solution period sequentially.
#' The solution is stored in the \code{IsisMdl} object, and can be
#' retrieved by methods \code{\link{get_data}}
#' (or \code{\link{get_ca}} for the constant adjustments).
#' Any subsequent solves of a model will use these data.
#' If a \code{solve} has converged and no data have changed,
#' then a second \code{solve} will report convergence in 0
#' iterations.
#'
#' The solve options specified are only applied to the current
#' solve. If none are specified the solve options
#' as specified with method \code{\link{set_solve_options}}
#' are used.
#'
#' The solve procedure \emph{never} raises an error, even if the solve was
#' not successful. In that case a warning may be issued. It is up to the user
#' to perform any checks.
#' Method \code{\link{get_solve_status}} can be used to check
#' whether the solve was successfully terminated or not.
#' The solve method outputs a report which the user should check.
#'
#' @seealso \code{\link{set_solve_options}},
#' \code{\link{set_fit_options}} and \code{\link{get_solve_status}}
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$solve(options = list(report = "fullrep"))
#'
#' # solve the model for all periods before 2018Q1
#' mdl$solve(period = "/2017Q4")
#'
#' # solve the model for all quarters in 2017 (2017Q1/2017Q4)
#' mdl$solve(period = "2017")
NULL

#' \code{\link{IsisMdl}} method: Returns the solve status of the last model solve.
#' @name get_solve_status
#' @md
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the status
#' of the last model solve as a  text string. If the last model solve
#' was successful, it returns the string \code{"OK"}.
#'

#' @section Usage:
#' \code{IsisMdl} method:
#' \preformatted{
#' mdl$get_solve_status()
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Details:
#'
#' The possible return values are:
#'  * \code{"Method solve has not yet been called"}
#'  * \code{"OK"}
#'  * \code{"Simulation not possible"} (usually this means that exogenous
#'  or feedback variables have \code{NA} values)
#'  * \code{"Simulation stopped"} (it was not possible to find a solution)
#'  * \code{"Initial lags/leads missing/invalid. Simulation not possible"}
#'  * \code{"Invalid parameter values detected. Simulation not possible"}
#'  * \code{"Fair-Taylor has not converged"}
#'  * \code{"Out of memory. Simulation not successful"}
#'  * \code{"Unknown problem in solve. Simulation not successful"}
#'
#' @seealso \code{\link{solve}}
#' @examples
#' \dontrun{
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$set_values(NA, names = "y", period = "2017Q1")
#' mdl$solve()
#' if (mdl$get_solve_status() != "OK") {
#'    stop("Error solving the model. Check the warnings!")
#' }
#' }
NULL

#' \code{\link{IsisMdl}} method: Calculates missing model data.
#' @name fill_mdl_data
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' attempts to calculate missing data for endogenous
#' variables of a model by evaluating the active equations in solution order. By
#' default only identity equations are evaluated, but if argument
#' `include_frmls = TRUE` then also all active behavioural equations are evaluated.
#'
#' If also `frml` equations are evaluated, the fit targets specified
#' with for example method \code{\link{set_fix}}  are ignored: if the value of the
#' left hand side variable is missing a value is computed by evaluating
#' the equation.
#'
#' The procedure can be used to fill in data before and beyond the
#' model period (as set by method \code{set_period}) for as many
#' variables as possible.
#'
#' @section Usage:
#' \preformatted{
#' mdl$fill_mdl_data(period = mdl$get_data_period(),
#'                   report = c("period", "minimal", "no"),
#'                   include_frmls = TRUE)
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
#' \item{\code{include_frmls}}{A logical. For the default value `FALSE` only active identity
#' equations are evaluated. If `TRUE` all active equations, including the behavioural (frml)
#' equations, are evaluated.}
#' }
#'
#' @section Details:
#'
#' Argument \code{report} can be used to specify
#' the type of report about the number of replaced missing values.
#' Specify
#' \describe{
#' \item{\code{minimal}}{to get a minimal report. Only the total number
#' of replaced missing values is reported}
#' \item{\code{period}}{to get a report per period (default). For each period
#' the number of replaced missing values is reported}
#' \item{\code{no}}{to not generate a report}
#' }
#'
#' @seealso \code{\link{run_eqn}}.
#'
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#'
#' mdl$set_values(200, names = "t", period = "2017Q1")
#
#' mdl$fill_mdl_data(period = "2017Q1")
#' print(mdl$get_data(names = "yd"))
NULL

#' \code{\link{IsisMdl}} method: Fills model data and inverse solves
#' to determine starting values for the model, i.e. the lags of the variables
#' @name fill_mdl_data_solve
#' @description
#' This method extends \code{\link{fill_mdl_data}} by both filling missing model data
#' and inverse solving for starting values. Like \code{\link{fill_mdl_data}}, it
#' calculates missing data for endogenous variables by evaluating equations in
#' solution order. Additionally, it solves inversely to find starting values (typically
#' lagged variables) that would produce observed outcomes in later periods.
#'
#' This method performs inverse modeling: instead of solving the model forward from
#' known starting values to outputs, it solves backward to find the starting values
#' that would produce the observed outputs. This is particularly useful for calibration
#' when you need to determine historical values (lags) before your main model period
#' begins, based on observed data in the initial period.
#'
#' The method is designed for situations where:
#' - You have observed data for certain variables in an initial period
#' - You need to solve for lagged values to use as starting values
#' - Exogenous variables are already known for all periods
#' - You want to initialize the model properly before running forward simulations
#'
#' Note: This method solves for starting values, not for exogenous variables.
#' For solving exogenous variables, use the separate \code{solve_exo} method.
#'
#'
#' The function solves the inverse problem using
#' numerical optimization and returns the modified model.
#'
#' @param period A period range object specifying the time period for the solution.
#'   If missing, uses the model's data period obtained via `$get_data_period()`.
#' @param solve_df A data frame defining the solve specifications. Must contain
#'   the following columns:
#'   \describe{
#'     \item{solve_period}{Character or period object indicating when to solve}
#'     \item{observed_variable}{Name of the variable with observed data}
#'     \item{solve_variable}{Name of the variable to solve for (derive)}
#'     \item{group}{(Optional) Group identifier for solving multiple variables together.
#'       If omitted, each row is treated as a separate group}
#'     \item{initial_guess}{(Optional) Initial guess for the solve variable.
#'       Should be all numeric, including NA's.
#'       Defaults to `default_initial_guess` value.}
#'   }
#' @param default_initial_guess (Optional) For rows in solve_df that do not have an
#'       initial guess, the default initial guess value will be used.
#'       Default is 0.1.
#' @param report Character string controlling output verbosity. Options:
#'   \describe{
#'     \item{"period"}{Print a report per period (default).
#'     For each period the number of replaced missing values is reported.}
#'     \item{"minimal"}{Print a minimal report.
#'     Only the total number of replaced missing values is reported.}
#'     \item{"no"}{Does not generate a report.}
#'     Defaults to `period` if not provided.
#'   }
#' @param ... Additional arguments passed to the `nleqslv` solver (e.g., `control`,
#'   `method`, `global`).
#'
#' @return An `IsisMdl` object (copy of the original model) with solved values for
#'   the solve variables. The original model remains unchanged.
#'
#' @examples
#' library(isismdl)
#' mdl_file <- tempfile(fileext = ".mdl")
#' writeLines("
#' ident y = y(-1);
#' ident obs = y + z;
#' ", mdl_file)
#'
#' # We want to solve the model in  period 2017/2018
#' mdl <- isis_mdl(mdl_file, period = "2017/2018", silent = TRUE)
#'
#' # We need the set the data period to period 2015/2018 to run this example.
#' # TODO: we have to fix this problem in the code of fill_mdl_data.
#' # See issue on Gitlab.
#' mdl$init_data("2015/2018")
#'
#' # Create initial data: z is known, obs is observed only in 2016
#' mdl$set_values(6:8, names = "z", period = "2016/2018")
#' mdl$set_values(100, names = "obs", period = "2016")
#'
#' mdl_copy <- mdl$copy() # Using in a later example
#'
#' print(mdl$get_data())
#'
#' # We cannot solve starting from 2017, because y(2016) is missing
#' mdl$solve()
#'
#' # Solve for y(2016) based on obs(2016) = 100
#' library(tibble)
#' solve_df <- tribble(
#'   ~solve_period, ~group, ~observed_variable, ~solve_variable, ~initial_guess,
#'   "2016", "A", "obs", "y", 0.1
#' )
#' mdl$fill_mdl_data_solve(
#'   period = "2016",
#'   solve_df = solve_df,
#'   report = "period"
#' )
#'
#'
#' print(mdl$get_data())
#'
#' # Now we can solve starting from 2017
#' mdl$solve()
#'
#' # Example with custom solver options
#' mdl_copy$fill_mdl_data_solve(
#'   solve_df = solve_df,
#'   report = "period",
#'   control = list(trace = 1, maxit = 200)
#' )
#'
#' # Clean up
#' unlink(mdl_file)
#'
#' @seealso
#' Related methods: \code{\link{solve}}, \code{\link{fill_mdl_data}}, \code{\link{get_dep_struct}}
#' \code{\link[nleqslv]{nleqslv}} for details on the numerical solver and options.
NULL

#' \code{\link{IsisMdl}} method: runs model equations
#' @name run_eqn
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} runs specific equations of the
#' model separately for the specified period range. The right-hand sides of the
#' equations are evaluated and used to update the values of the
#' corresponding left-hand side variables in the model data.
#'
#' If the equation is a stochastic equation (a \code{frml} equation)
#' and the corresponding endogenous variable has been fixed,
#' then the constant adjustment of the equation will be calculated
#' such that the result of the equation equals the predetermined required
#' value for the left-hand side.
#'
#' The names of the equations to be run can be specified with argument
#' `names` or `pattern`. These arguments cannot be specified both.
#' If neither argument \code{pattern} nor \code{names} has been specified,
#' then all active model equations are run.
#'
#' @section Usage:
#' \preformatted{
#' mdl$run_eqn(pattern, names, period = mdl$get_data_period(),
#'             solve_order, forwards = TRUE, update_mode = c("upd", "updval"),
#'             by_period = FALSE)
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression. Equations with names
#' matching the regular expression are run.}
#' \item{\code{names}}{a character vector with equation names}
#' \item{\code{period}}{a \code{\link[regts]{period_range}} object
#' or a single \code{\link[regts]{period}} specifying the period range.
#' By default the equation are run for the whole data period.}
#' \item{\code{solve_order}}{
#' A logical: should the specified equations be run in solve order?
#' The default value depends on whether argument `names` has been
#' specified: `FALSE` if `names` has been specified and otherwise `TRUE`.
#' See Section Equation Order.}
#' \item{\code{forwards}}{A logical indicating whether the equations
#' are evaluated forwards or backwards in time.
#' See Section Forwards and Backwards.}
#' \item{\code{update_mode}}{This argument specifies whether the model data
#' should be updated with the result of running an equation
#' if the result is an invalid number (`NA`). If `update_mode = "upd"`
#' (the default), the model data is always updated with the result.
#' If `update_mode = "updval"`, the model data is only updated if the
#' result is not `NA`.}
#' \item{\code{by_period}}{A logical (default `FALSE`).
#' If `TRUE`, and if `forwards` is `TRUE`, all equations are first evaluated at
#' the first period, then all equations at the second period, and so on. If
#' `by_period` is `FALSE`, the first equation is first run for all periods
#' (starting at the first period,  then the second period etc.), then the second
#' equation is solved for all periods, and so on.
#' If `by_period` is `TRUE` and if `forwards` is `FALSE`, all equations are first
#' evaluated at the last period, then all equations at the last but one period,
#' and so on. If `by_period` is `FALSE`, the first equation is first run for
#' all periods (starting at the last period, then the last but one period etc.),
#' then the second equation is solved for all periods, and so on.}
#' }
#'
#' Only one of the two arguments `pattern` and `names` can be specified.
#'
#' @section Equation order:
#'
#' If argument `solve_order = TRUE`, the specified equations
#' are run in solve order, i.e. the order used when solving the model.
#' If `solve_order = FALSE`, the order depends on whether argument `names`
#' is specified:
#'   * If argument `names` has been specified, the equations are run
#' in the same order as the specified names.
#'   *  Otherwise the equations are run using the 'natural order',
#' i.e. the order of the equations as defined in the model file.
#' The default of argument `solve_order` is `FALSE` if `names` has been
#' specified and `TRUE` in other cases.
#'
#' @section Forwards and Backwards:
#'
#' By default, the equations are run forwards in time: the equations are
#' first evaluated at the first period of the specified period range,
#' then at the second period, and so on.
#' If argument `forwards = FALSE`, the equations are run backwards:
#' first at the last period of the specified period range,
#' then at the last but one period, and so on.
#'
#' @seealso \code{\link{solve}} and  \code{\link{fill_mdl_data}}.
#
#' @examples
#' mdl <- islm_mdl("2017Q1/2019Q3")
#' mdl$run_eqn(names = c("c", "t"))
#'
#' # run all equations with names starting with y
#' mdl$run_eqn(pattern = "^y")
#'
#' # run all model equations in the order of the equations as specified
#' # in the mdl file
#' mdl$run_eqn(solve_order = FALSE)
#'
#' # emulate a single pass through the model
#' # note that we use by_period = TRUE
#' mdl$run_eqn(period = mdl$get_period(), by_period = TRUE)
NULL


#' \code{\link{IsisMdl}} methods: Retrieve timeseries from the model data,
#' constant adjustments, fix values or fit targets
#' @name get_data-methods
#' @aliases get_data get_ca get_fix get_fit
#' @description
#' These methods of R6 class \code{\link{IsisMdl}}
#' can be used to retrieve timeseries from the model data,
#' constant adjustments, fix values or fit targets.
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
#' object that can be coerced to a \code{period_range}.
#' The frequency of `period` should be equal to or lower than the
#' frequency of the model period. If the frequency is lower,
#' than the period range is converted to the same frequency
#' as the model frequency with function \code{\link[regts]{change_frequency}}
#' of package `regts`.}
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
#'
#' @examples
#' mdl <- islm_mdl(period = "2016Q1/2017Q4")
#'
#' print(mdl$get_data())
#'
#' # print data for 2017Q2 and later
#' print(mdl$get_data(names = c("g", "y"), period = "2017Q2/"))
#'
#' # print data for all quarters in 2017 (2017Q1/2017Q4)
#' print(mdl$get_data(names = c("g", "y"), period = 2017))
#'
#' print(mdl$get_data(pattern = "^ymdl"))
#'
NULL

#' \code{\link{IsisMdl}} methods: transfers data from a timeseries
#' object to the model data, constant adjustments, fix values or fit targets.
#' @name set_data-methods
#' @aliases set_data set_ca set_fix set_fit
#' @description
#' These methods of R6 class \code{\link{IsisMdl}}
#' Transfers data from a timeseries object to the model data,
#' constant adjustments, fix values or fit targets.
#' @section Usage:
#' \preformatted{
#' mdl$set_data(data, names = colnames(data), upd_mode = c("upd", "updval"),
#'              fun, name_err = c("silent", "warn", "stop"))
#'
#' mdl$set_ca(data, names = colnames(data), upd_mode = c("upd", "updval"),
#'            fun, name_err = c("silent", "warn", "stop"))
#'
#' mdl$set_fix(data, names = colnames(data), upd_mode = c("upd", "updval"),
#'             name_err = c("silent", "warn", "stop"))
#'
#' mdl$set_fit(data, names = colnames(data), upd_mode = c("upd", "updval"),
#'             name_err = c("silent", "warn", "stop"))
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{data}}{a \code{\link[stats]{ts}} or \code{\link[regts]{regts}}
#'  timeseries object}
#' \item{\code{names}}{a character vector with variable names, with the
#' same length as the number of timeseries in \code{data}. Defaults to the
#' column names of \code{data}. If \code{data} does not have column names,
#' then argument \code{names} is mandatory}
#' \item{\code{upd_mode}}{the update mode, a character string specifying
#' how the timeseries are updated: \code{"upd"} (standard update, default) or
#' \code{"updval"} (update only with valid numbers). See details.}
#' \item{\code{fun}}{a function used to update the model data. This should
#' be a function with two arguments. The original model data is passed to the first
#' argument of the function and \code{data} to the second argument.
#' See the examples.}
#' \item{\code{name_err}}{A character that specifies the
#' action that should be taken when a name is not the name model variable
#' of the appropriate type (for `set_fit`, the variable must be endogenous,
#' for `set_fix` a frml variable).
#' For `"silent"` (the default), the variable is silently skipped,
#' for `"warn"` a warning is given and for `"stop"` an error is  issued.}
#' }
#' @section Methods:
#'
#' \describe{
#' \item{\code{set_data}}{Sets model data.
#' If \code{data} has labels, then \code{set_data} will also update
#' the labels of the corresponding model variables}
#' \item{\code{set_ca}}{Set constant adjustments, i.e. the residuals of
#' behavioral (frml) equations}
#' \item{\code{set_fix}}{Set fix values for frml variables
#' (model variables that occur at the left hand side of a frml equation).
#' The frml variable is kept fixed
#' at the specified value, and the constant adjustment of the frml equation
#' is computed at the difference between the frml variable and the right hand
#' side of the equation. A fix value of \code{NA} implies
#' that the corresponding variable is *not* fixed. \code{set_fix}
#' also updates the model data with all non-NA values in `data`.}
#' \item{\code{set_fit}}{Set fit targets for the fit procedure.
#' A fit target value of \code{NA} implies
#' that the corresponding variable is no fit target}
#'}
#' @section Details:
#'
#' Method \code{set_data} transfers data from a timeseries object to the
#' model data. If \code{data} is a multivariate timeseries object, then
#' each column is used to update the model variable with the same
#' name as the column name. If \code{data} does not have column names,
#' or if the column names do not correspond to the model variable names,
#' then argument \code{names} should be specified.
#'
#' By default, all values in \code{data} are used to update the corresponding
#' model variable. Sometimes it is desirable to skip the \code{NA} values
#' in \code{data}. This can be achieved by selecting \code{"updval"} for argument
#' \code{upd_mode}. Other non finite numbers (\code{NaN}, \code{Inf}, and
#' \code{-Inf}) are also disregarded for this update mode.
#'
#' \code{set_ca}, \code{set_fix} and \code{set_fit} and
#' \code{set_data} work similarly.
#'
#' @examples
#'
#' mdl <- islm_mdl(period = "2017Q1/2017Q3")
#'
#' # create a multivariate regts object for exogenous variables g and md
#' exo <- regts(matrix(c(200, 210, 220, 250, 260, 270), ncol = 2),
#'              start = "2017Q1", names = c("g", "ms"))
#'
#' # set and print data
#' mdl$set_data(exo)
#' print(mdl$get_data())
#'
#' # create a univariate regts object for exogenous variable ms,
#' # with a missing value in 2017Q2
#' ms <- regts(c(255, NA, 273), start = "2017Q1")
#'
#' # update with update mode updval (ignore NA)
#' # note that here we have to specify argument names,
#' # because ms does not have column names
#' mdl$set_data(ms, names = "ms", upd_mode = "updval")
#' print(mdl$get_data())
#'
#' # in the next example, we use argument fun to apply an additive shock to the
#' # exogenous variables g and ms.
#' shock <- regts(matrix(c(-5, -10, -15, 3 , 6, 6), ncol = 2),
#'              start = "2017Q1", names = c("g", "ms"))
#' mdl$set_data(shock, fun = function(x1, x2) {x1 + x2})
#'
#' # the statement above can be more concisely written as
#' mdl$set_data(shock, fun = `+`)
#' #`+` is a primitive function that adds its two arguments.
#'
#'
#' # fix c in 2017Q1/2017q2 and i in 2017q1 to specific values
#' c <- regts(250, period = "2017q1/2017q2")
#' i <- regts(175, period = "2017q1")
#' fix_data <- cbind(c, i)
#' mdl$set_fix(fix_data)
#'
#' @seealso \code{\link{get_data-methods}}, \code{\link{set_values-methods}},
#' \code{\link{change_data-methods}}, \code{\link{fix_variables}},
#' \code{\link{clear_fix}} and \code{\link{clear_fit}}.
NULL

#' \code{\link{IsisMdl}} methods: Sets the values of the model data,
#' constant adjustments, fix values or fit targets
#' @name set_values-methods
#' @aliases set_values set_ca_values set_fix_values set_fit_values
#' @description
#' These methods of R6 class \code{\link{IsisMdl}}
#' can be used to set the values of the model data, constant adjustments,
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
#' \item{\code{value}}{A numeric vector of length 1 or with the same length
#' as the length of the range of \code{period}.}
#' \item{\code{names}}{A character vector with variable names.
#' For `set_ca_values` and `set_fix_values`, the names should be the
#' names of frml variables (the variables on the left hand side of frml equations).
#' For `set_fit_values`, the names should be the names of endogenous variables.}
#' \item{\code{pattern}}{A regular expression specifying the
#' variable names.}
#' \item{\code{period}}{A \code{\link[regts]{period_range}} object or an
#' object that can be coerced to a \code{period_range}. The default
#' is the data period. The frequency of `period` should be equal to or lower than the
#' frequency of the model period. If the frequency is lower,
#' than the period range is converted to the same frequency
#' as the model frequency with function \code{\link[regts]{change_frequency}}
#' of package `regts`.}
#' }
#' If neither \code{names} nor \code{pattern} has been specified, then the action
#' is applied to all model variables of the appropriate type.
#' @section Methods:
#' \describe{
#' \item{\code{set_values}}{Sets the values of model data.}
#' \item{\code{set_ca_values}}{Sets the values of the  constant adjustments, i.e. the
#' residuals of 0 (frml) equations.}
#' \item{\code{set_fix_values}}{Set fix values for the stochastic
#' model variables (i.e. model variables that occur at the left
#' hand side of a frml equation). The model variables will be fixed
#' at the specified value. A fix value of \code{NA} implies
#' that the corresponding variable is not fixed. \code{set_fix}
#' also updates the model data with all non NA values.}
#' \item{\code{set_fit_values}}{Set fit targets for the fit procedure.
#' A fit target value of \code{NA} implies
#' that the corresponding variable is no fit target.}
#' }
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2017Q3")
#'
#' # set the values for y in the full data period
#' mdl$set_values(1000, names = "y")
#'
#' # set the values of ms and md in 2017Q1 and 2017Q2
#' mdl$set_values(c(205, 206), pattern = "^m.$", period = "2017Q1/2017Q2")
#'
#' # set the values of ms and md in all quarters of 2017 (2017Q1/2017Q4)
#' mdl$set_values(c(205, 206, 207, 208), pattern = "^m.$", period = "2017")
#
#' print(mdl$get_data())
#'
#' # give the constant adjustment of variable c the value 1
#' mdl$set_ca_values(0, names = "c")
#'
#' # fix c and i at 200 in period 2017q1/2017q2
#' mdl$set_fix_values(200, names = c("c", "i"))
#'
#' @seealso \code{\link{get_data-methods}}, \code{\link{set_data-methods}}
#' and \code{\link{change_data-methods}}
NULL

#' \code{\link{IsisMdl}} methods: changes the model data or constant
#' adjustments by applying a function.
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
#' \item{\code{fun}}{a function applied each model timeseries or
#' constant adjustment specified with argument \code{names} or \code{pattern}}
#' \item{\code{names}}{a character vector with variable names}
#' \item{\code{pattern}}{a regular expression}
#' \item{\code{period}}{an \code{\link[regts]{period_range}} object or an
#' object that can be coerced to a \code{period_range}}
#' \item{\code{...}}{arguments passed to \code{fun}}
#' }
#'
#' @section Details:
#'
#' The function specified with argument `fun` should be a function
#' with at least one argument, for example `fun = function(x) {x + 0.1}`.
#' The first argument (named `x` in the example) will be the model
#' variable. The function is evaluated for each model variable separately.
#' The values of the model variables for period range `period` are passed as a
#' normal numeric vector (not a timeseries) to the first argument.
#'
#' An example may help to clarify this. Consider the following statement
#'  ```
#'  mdl$change_data(fun = myfun, names = c("c", "y"),
#'                       period = "2017q1/2017q2"),
#'  ```
#'
#'  where `mdl` is a `DynMdl` object and `myfun` some function whose details
#'  are not relevant here. Method  \code{change_data} evaluates this as
#'  ```
#'  data <- mdl$get_data(names = c("c", "y"), period = "2017q1/2017q2")
#'  data[, "c"] <- myfun(as.numeric(data[, "c"]))
#'  data[, "y"] <- myfun(as.numeric(data[, "y"]))
#'  mdl$set_data(data)
#'  ```
#'
#'  The function result must be a vector (or timeseries) of length one or with
#'  the same length as the number of periods in the period range \code{period}.
#'
#' @section Methods:
#' \describe{
#' \item{\code{changes_data}}{Changes the model data}
#' \item{\code{change_ca}}{Changes the constant adjustments}
#' }
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2017Q3")
#'
#' # increase y and yd with 10% for the full data period
#' mdl$change_data(pattern = "^y.?$", fun = function(x) {x * 1.1})
#'
#' # increase ms in 2017Q1 and 2017Q2 with 10 and 20, resp.
#' mdl$change_data(names = "ms", fun = function(x, dx) {x + dx},
#'                 dx = c(10, 20), period = "2017Q1/2017Q2")
#' print(mdl$get_data())
#'
#' @seealso \code{\link{get_data-methods}}, \code{\link{set_data-methods}} and
#' \code{\link{set_values-methods}}
#'
NULL

#' \code{\link{IsisMdl}} method: Sets the root mean square errors
#' @name set_rms
#' @aliases get_rms set_rms_values
#'
#' @description
#' Methods `set_rms` and `set_rms_values` of R6 class \code{\link{IsisMdl}}
#' can be used to set the root mean square (rms) error values
#' used in the fit procedure.
#' Each frml equation has a constant adjustment and a corresponding rms value.
#' If the rms value is larger than 0 and not \code{NA}, then the constant
#' adjustment is used as a fit instruments.
#'
#' Method `set_rms` can be used to set individual rms values,
#' while `set_rms_values` is a convenient method to give more than one rms value
#' the same value.
#'
#' Method \code{get_rms} returns all rms values larger than 0 and not equal
#' to \code{NA}.
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_rms(values, name_err = c("warn", "stop", "silent"))
#'
#' mdl$set_rms_values(value, names, pattern)
#'
#' mdl$get_rms()
#'
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object

#' @section Arguments:
#'
#' \describe{
#' \item{\code{values}}{A named numeric vector with rms values.
#' The names should be the names of the corresponding frml variables
#' (the variables at the left hand side of a frml equation).}
#' \item{\code{name_err}}{A character that specifies the
#' action that should be taken when a name is not the name of a frml variable.
#' For `"warn"` (the default), a warning is given, for `"stop"` an error is
#' issued. For `"silent"`, the variable is silently skipped,
#' }
#' \item{\code{value}}{A numeric vector of length 1.}
#' \item{\code{names}}{A character vector specifying the names of the frml
#' variables.}
#' \item{\code{pattern}}{A regular expression. The action (get or
#' set rms values) is applied to the rms values corresponding to the
#' frml variables with names matching \code{pattern}.}
#' }
#' If neither \code{names} nor \code{pattern} has been specified in
#' methods \code{set_rms_values} or \code{get_rms}, then the action
#' is applied to all rms values.
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#'
#' mdl$set_rms(c(c = 5.0, t = 2, i = 21, md = 2))
#' print(mdl$get_rms())
#'
#' # remove the constant adjustment for variable c from the fit instruments
#' mdl$set_rms_values(NA, "c")
#' print(mdl$get_rms())
#'
#' # make all rms values equal to 1
#' mdl$set_rms_values(1)
#'
#' # set the rms values for c and i to 2
#' mdl$set_rms_values(2, names = c("c", "i"))
NULL

#' \code{\link{IsisMdl}} method: Sets the solve options
#' @name set_solve_options
#' @aliases get_solve_options
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} can be used to set one or more
#' solve options. These options will be stored in the \code{IsisMdl} object.
#'
#' Method \code{get_solve_options} returns the solve options as a named
#' list
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_solve_options(mode, fbstart, maxiter, maxjacupd, rlxspeed,
#'                       rlxmin, rlxmax, cstpbk, cnmtrx, xrelax,
#'                       xmaxiter, xupdate, dbgopt, erropt,
#'                       report, ratreport, ratreport_rep, ratfullreport_rep,
#'                       bktmax, xtfac, svdtest_tol)
#'
#' mdl$get_solve_options()
#' }
#'
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
#' the method of initializing feedback values.
#' (\code{"current"}, \code{"previous"}, \code{"curifok"} or \code{"previfok"}).
#' The default is \code{"current"}. See section "Feedback initialization
#' methods" below}
#' \item{\code{maxiter}}{the maximum number of iterations per period (default 50)}
#' \item{\code{maxjacupd}}{the maximum number of Newton Jacobian updates per period
#'  (default 10)}
#'  \item{\code{rlxspeed}}{Newton relaxation shrinkage (default is 0.5)}
#'  \item{\code{rlxmin}}{Minimum Newton relaxation factor (default is 0.05)}
#'  \item{\code{rlxmax}}{Maximum Newton relaxation factor (default is 1.0)}
#'  \item{\code{cstpbk}}{Stepback criterion (default is 1.3).
#' If the convergence criterion \code{Fcrit} is larger
#' than \code{cstpbk} or invalid feedback variables
#' have been obtained then the Newton step
#' is not accepted and linesearching will be initiated.
#' If the linesearching procedure failed
#' (\code{Fcrit} is still larger than \code{cstpbk}
#' after the maximum number of linesearch steps \code{bktmax}
#' has been reached or if the relaxation
#' factor has become smaller than \code{rlxmin}),
#' a new Jacobean matrix is computed.
#' In each linesearch step the current relaxation factor is shrunk by
#' \code{rspeed}.
#' The relaxation factor is set to its maximum value
#' \code{rlxmax}) when a new Jacobian has been calculated.
#' }
#'  \item{\code{cnmtrx}}{Recalculate matrix criterion (default is 0.9).
#' If the convergence criterion \code{Fcrit} is larger
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
#' invalid lags, leads, constant adjustments  and/or exogenous variables are detected.
#' Possible values are \code{"stop"} (stop on errors),
#' \code{"cont"} (continue on errors but write a message to the output)
#' and \code{"silent"} (also continue but without message).
#' The default is \code{"stop"}}
#' \item{\code{report}}{A character string defining the type of
#' computation progress report. Possible values are
#' \code{"period"} (for a report per period),
#' \code{"fullrep"} (for a full report),
#' \code{"minimal"}  (for a minimal report), and
#' \code{"none"}  (for no report). The default is \code{"period"}.
#' The report options \code{"none"} also suppresses all output
#' of the fit procedure and the Fair-Taylor progress report.}
#' \item{\code{ratreport}}{Defines the type of rational expectations progress
#' report. See section "Ratex report options" below}
#' \item{\code{ratreport_rep}}{An integer number specifying
#' the Fair-Taylor report repetition count.
#' See Section "Ratex report options" below.}
#' \item{\code{ratfullreport_rep}}{An integer number, specifying
#' the Fair-Taylor full report repetition count. See Section
#' "Ratex report options" below.}
#' \item{\code{bktmax}}{Maximum number of backtracking linesearch steps
#' with old Jacobian. Sometimes it is necessary for the Broyden
#' method to take a shorter step than the standard step. This is called
#' backtracking linesearch. \code{bktmax} is the maximum number of
#' line search steps before a new Jacobian is computed.}
#' \item{\code{xtfac}}{Rational expectations convergence test multiplier
#' When using the \code{"ratex"} solution mode,
#' convergence of endogenous leads cannot be tested to the accuracy used in
#' testing for convergence in the solution of the model.
#' This option specifies the multiplier to apply to the convergence criterion
#' for each endogenous variable if the variable has an endogenous lead.
#' Suppose for example that some variable has a convergence criterion of `1e-5`
#' and assume a value of 10 for the multiplier.
#' Then its endogenous lead will be regarded as converged.}
#' \item{\code{svdtest_tol}}{Singular Value Decomposition (SVD) test tolerance
#' parameter.
#' If the inverse condition of the Jacobian is smaller than this parameter,
#' then an SVD analysis of the Jacobian is performed. This may help to
#' find the equations that cause (near) singularity of the Jacobian.
#' The default value is \code{-1}, which implies that the SVD test is never
#' performed. Specify a number between 0 and 1 to enable an SVD analysis depending
#' on the inverse condition of the Jacobian.
#' When this option has been specified a copy of the Jacobian is kept in memory,
#' even if the Jacobian is not ill-conditioned.
#' This option should therefore only be used during testing. It should be turned
#' off in production calculations.}
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
#' @section Feedback initialization methods:
#'
#' Argument \code{fbstart} can be used to specify
#' the way how the feedback variables at the current period
#' (i.e. the period for which the model is being solved)
#' are initialized from the model data.
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
#' \item{\code{"prijac"}}{print Jacobian matrix when updated}
#' \item{\code{"prinoconv"}}{print all not converged endogenous variables}
#' \item{\code{"prinotconvl"}}{print all not converged leads}
#' \item{\code{"allinfo"}}{all of the above}
#' \item{\code{"noprifb"}}{do not print feedback variables at each iteration}
#' \item{\code{"noprild"}}{do not print all leads at each ratex iteration}
#' \item{\code{"noprijac"}}{do not print Jacobian matrix when updated}
#' \item{\code{"noprinoconv"}}{print only the largest discrepancy of
#' all not converged endogenous variables}
#' \item{\code{"noprinotconvl"}}{print only the largest discrepancy of
#' all not converged leads}
#' \item{\code{"noinfo"}}{no debugging output}
#' \item{\code{"priscal"}}{print scaling factors as determined from the
#' Jacobian}
#' \item{\code{nopriscal}}{do not print scaling factors as determined
#' from the Jacobian}
#' }
#' Default is no printing of debugging information.

#' @section Ratex report options:
#'
#' The type of report is determined by argument \code{ratreport}.
#' Arguments \code{ratreport_rep} (the report repetition count)
#' and \code{ratfullreport_rep} (the full report repetition count),
#' both specified as integer numbers,
#' can be used to further modify the progress report.
#'
#' Possible values for \code{ratreport} are
#' \describe{
#' \item{\code{"iter"}}{print the number of not converged
#' expectation values every \code{ratreport_rep} Fair-Taylor iteration
#' (the default)}
#' \item{\code{"fullrep"}}{full report. The number of not converged
#' expectation values is printed every \code{ratreport_rep} Fair-Taylor
#'  iteration and the largest remaining discrepancy every
#' \code{ratfullreport_rep} Fair-Taylor iteration}
#' \item{\code{"minimal"}}{for a full report only after the last Fair-Taylor
#' iteration}
#' }
#'
#' If \code{ratfullreport_rep} is \code{NA}, then the full report
#' is printed every \code{ratreport_rep} Fair-Taylor iteration.
#' The default values for \code{ratreport_rep}  and \code{ratfullreport_rep}
#' are 1 and \code{NA}, respectively.
#'
#' @seealso \code{\link{set_debug_eqn}}
#' @examples
#' mdl <- islm_mdl(period = "2017Q1/2018Q4")
#' mdl$set_solve_options(maxiter = 100)
NULL

#' \code{\link{IsisMdl}} method: Sets the options for the fit procedure.
#' @name set_fit_options
#' @aliases get_fit_options
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} can be used to set one or more
#' options for the fit procedure.
#' These options will be stored in the \code{IsisMdl} object.
#'
#' Method \code{get_fit_options} returns the solve options as a named list
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_fit_options(maxiter, cvgabs, mkdcrt, cvgrel, zero_ca, warn_ca,
#'                    accurate_jac, zealous, scale_method,
#'                    warn_zero_row, warn_zero_col,
#'                    chkjac, report, dbgopt, svdtest_tol)
#'
#' mdl$get_fit_options()
#'
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
#' \item{\code{mkdcrt}}{Criterion for calculating a new fit Jacobian.
#' When the ratio of two successive largest scaled discrepancies of
#' the fit target values is
#' larger than \code{mkdcrt} a new fit Jacobian will be calculated
#' in the next iteration. Any value specified must lie between 0.05
#' and 0.95. The default value is 0.5.}
#' \item{\code{cvgrel}}{Criterion for accepting the result of a fit
#' iteration (default 0.95).
#' When the ratio of two successive largest scaled discrepancies of
#' the fit target values is
#' larger than \code{cvgrel},  then the result of the iteration is rejected.
#' If the iteration employed an old fit Jacobian (i.e. a Jacobian
#' computed in  an earlier iteration), then a second attempt with
#' a  new Jacobian is made. If the iteration already used a new
#' Jacobian, then the fit procedure will be terminated.}
#' \item{\code{zero_ca}}{A logical. If \code{TRUE}, then the initial values
#' of the constant adjustments used in the fit procedure are initialized to 0.
#' The default is \code{FALSE}}
#' \item{\code{warn_ca}}{A logical. If \code{TRUE} (default), then warnings
#' are given for possibly too large constant adjustments at the end of the fit
#' procedure for each period.}
#' \item{\code{accurate_jac}}{A logical. If \code{TRUE} (default), then the
#' fit Jacobian is calculated accurately, otherwise the Jacobian
#' is calculated approximately. See Details.}
#' \item{\code{zealous}}{A logical. If \code{TRUE} (default), then a zealous
#' version of the fit procedure is used (see section The Zealous and Lazy Fit Method),
#' otherwise a lazy version is used.
#' The recommended option is to use the zealous version,
#' although this may require much more CPU time.}
#' \item{\code{scale_method}}{The scaling method for the fit Jacobian.
#' Possible values are `"row"` (row scaling, the default), and `"none"` (no scaling).
#' See Section "Row Scaling".}
#' \item{\code{warn_zero_row}}{A logical (default `FALSE`). If `TRUE`, then a
#' warning is issued for each row of the fit Jacobian for which all values are
#' almost equal to zero. A row of the fit Jacobian contains the derivatives
#' of a fit targets with respect to the residuals. A row is considered
#' almost zero if the L1-norm of that row is smaller than a fraction \eqn{\epsilon} of
#' the largest L1-norm of the rows. \eqn{\epsilon} is the square root of
#' the machine precision (typically \code{1.5e-8}).
#' If row scaling is applied (see argument `scale_method`), then a row that is
#' almost zero is usually not problematic. However,
#' if all values in a row are *exactly* zero, then the fit procedure is not
#' possible and therefore an message is always issued,
#' even if `warn_zero_row = FALSE`.}
#' \item{\code{warn_zero_col}}{A logical (default `FALSE`). IF `TRUE`, then a
#' warning is issued for each column of the fit Jacobian for which all values are
#' almost or exactly equal to zero. A column of the fit Jacobian contains the derivatives
#' of all fit targets with respect to one particular residual. A columns is considered
#' almost zero if the L1-norm of that column is smaller than a fraction
#' \eqn{\epsilon} of the largest L1-norm of the columns. \eqn{\epsilon} is the
#' square root of the machine precision (typically \code{1.5e-8}).
#' A column that is (almost) zero is not necessarily problematic, except when
#' the number of non-zero columns is smaller than the number of rows (the number
#' of fit targets). In particular, if the number of columns with a norm
#' exactly equal to zero is larger than the difference between the number of
#' rows and the number of columns, then the fit procedure is not possible. Therefore
#' a message about zero columns is always given in that case.}
#' \item{\code{chkjac}}{A logical. If `TRUE` (the default), then the fit
#' method is terminated when the inverse condition of the fit Jacobian
#' is smaller than the square root of the machine precision
#' (typically \code{1.5e-8}).
#' When a model is badly scaled, the inverse condition number of the
#' Jacobian may become small, which can lead to inaccurate or even unstable
#' solutions. If `FALSE`, the fit procedure is only terminated when the
#' inverse condition is exactly zero.}
#' \item{\code{report}}{A character string specifying the the
#' type of report of the fit procedure for each period.
#' Possible values are \code{"fullrep"}
#' (the default, an iteration report is printed for each period)
#' and  \code{"minimal"} (for a one line summary).}
#' \item{\code{dbgopt}}{A character vector specifying one or more debugging
#' options. See section "Debugging Options" below}
#' \item{\code{svdtest_tol}}{Singular Value Decomposition (SVD) test tolerance
#' parameter. The default value for argument `svdtest_tol` is \code{-1},
#' which implies that the SVD test is never performed.
#' Specify a number between 0 and 1
#' to enable an SVD analysis, depending on the inverse condition of the Jacobian.
#' See section "SVD Analysis'.
#' If scaling has been applied (see argument `scale_method`), then the SVD
#' analysis is performed for the scaled Jacobian.
#' Sometimes it is easier to interpret the result of the SVD analysis by turning
#' off row scaling.
#' When this option has been specified, a copy of the fit Jacobian is kept in memory,
#' even if the Jacobian is not ill-conditioned.
#' For large models this option should therefore only be used during testing,
#' and should be turned off in production calculations}
#' }
#'
#' @section Details:
#' The purpose of the fit procedure is to adjust a model solution
#' to a partial set of known outcomes for endogenous variables. It
#' determines the minimal norm vector of specified constant
#' adjustments which ensure that the specified endogenous variables
#' meet the desired outcome (fit targets).
#' It can be used amongst others to update a model forecast given a
#' (small) set of recent observations of endogenous variables.
#' It first solves the model for any period given all data and if
#' fit targets have been specified then proceeds to determine a set
#' of constant adjustments that will ensure that the fit targets
#' are met. There must be at least as many constant adjustments as there are fit
#' targets for the fit procedure to work.
#'
#' After solving the model in any period for a given set of values
#' of the constant adjustments (residuals), the fit problem can be
#' described as follows. Find a minimum norm vector \eqn{u} such that
#'
#' \deqn{y = h(u) = w}
#'
#' where \eqn{u} is an \eqn{n}-vector of scaled residuals,
#' \eqn{y} is an \eqn{m}-vector of
#' endogenous variables with \eqn{n >= m},
#' \eqn{h} the function \eqn{h: R^n \rightarrow R^m}
#' and \eqn{w} is an \eqn{m}-vector of fit target values.
#' The scaled residuals \eqn{u_i} have been scaled with the root mean square
#' values specified with procedures \code{\link{set_rms}}.
#'
#' The fit procedure linearizes the relation \eqn{y=h(u)} and
#' determines a minimum norm solution for \eqn{u} to the resulting set of
#' linear equations after setting \eqn{y=w}.
#' It uses the QR decomposition for numerical stability.
#'
#' The fit Jacobian \eqn{D_{ij} = \partial h_i / \partial u_j} is calculated
#' numerically by a first difference approach.
#' The \eqn{j}'th column is calculated by giving residual \eqn{u_j} a small
#' distortion and then solving the model again.
#' For numerical efficiency the model is solved with a *single* iteration
#' by default. This is usually a good approximation.
#' Use argument `accurate_jac = TRUE` for a more accurate
#' calculation of the fit Jacobian. For this option the model is solved
#' until convergence has been reached.
#'
#' The criterion used for testing for convergence is the largest
#' scaled discrepancy of the fit target values at iteration \eqn{k}
#' defined as
#
#' \deqn{
#' F_k = \max_i \left\{ | w_i - y_i |  / \max(|w_i|,1) \right\}
#' }
#'
#' When \eqn{F_k \le \epsilon} where \eqn{0 < \epsilon < 1},
#' absolute convergence has been achieved.
#' The value of \eqn{epsilon} is specified with argument `cgvabs` (
#' (the default value is 100 times the square root of
#' the machine precision, which is typically \code{1.5e-6}).
#' If the zealous fit method is used (see Section The Zealous
#' and Lazy Fit Method), we also require for convergence that
#' the relative step size for all variables is smaller than \eqn{epsilon}.
#'
#' Since evaluating the Jacobian of \eqn{h(u)} can be a time-consuming
#' process, the Jacobian of a previous iteration can sometimes be reused for
#' a next iteration.
#' As long as \eqn{F_k \le \delta  F_{k-1}} where \eqn{0 < \delta < 0.95}
#' the current Jacobian will not be recalculated, except when the zealous
#' fit method is used and the the number
#' of residuals is larger than the number of targets, see Section The Zealous
#' and Lazy Fit Method.
#' The default value for \eqn{\delta} is 0.5.
#' When \eqn{F_k > 0.95 F_{k-1}}
#' and the current Jacobian is not
#' up-to-date, the residuals will be reset to the values of the
#' previous iteration and the Jacobian will be recalculated.
#' However if the current Jacobian is up-to-date, the process will be
#' stopped with the message \code{Cannot locate a better point}.
#'
#' If the zealous fit method is used (see next paragraph), then
#' a new Jacobian is calculated every iteration when the number
#' of residuals is larger than the number of targets (\eqn{m > n}).
#'
#' @section The Zealous and Lazy Fit Method:
#'
#' There are two implementations of the fit procedure: the lazy
#' and zealous method. The default is the zealous method.
#' For the lazy method the fit iterations is terminated when
#' the largest scaled discrepancy of the fit target values
#' is less than `cvgabs`. However,
#' the other variables may not be converged yet sufficiently, particularly
#' when the number of residuals is larger than the number of targets (\eqn{m > n}).
#' For the zealous fit procedure continues iterating
#' until the relative changes of all variables are less than `cvgabs`.
#' These relative changes are shown
#' in the output as `Delsmx` (maximum step size in an iteration).
#' The zealous fit procedure also uses an accurate calculation
#' of the Jacobian (see general description).
#' If \eqn{m > n} (non-square fit problem), the zealous fit procedure
#' also updates the fit Jacobian every iteration,
#' because for non-square fit problems the results depends on the Jacobian.
#' For the square case \eqn{m = n} this is not necessary because the
#' final results are independent on the Jacobian.
#'
#' @section Row scaling:
#'
#' As explained in section Details, the fit Jacobian \eqn{D_{ij}} is a matrix
#' with the derivatives of the fit targets (\eqn{i}) with respect to the scaled
#' residuals (\eqn{j}). If there are large scale differences between the
#' fit targets, additional row scaling may improve the condition
#' number of the fit Jacobian.
#'
#' The following procedure is
#' used to determine if row scaling is necessary. For each row \eqn{i},
#' the maximum absolute values \eqn{R_i} is determined. If
#' if the ratio of the largest and smallest value of vector \eqn{R} is larger
#' than 10, then all rows are scaled so that
#' the largest absolute value in each row is 1. If the ratio is smaller than
#' 10, the Jacobian is not scaled.
#'
#' Row scaling can be turned off by specifying argument `scale_method = "none"`.
#'
#' @section Debugging Options:
#'
#' Argument \code{dbgopt} can be used to specify one or more
#' options for debugging the fit procedure.  Possible values are
#' \describe{
#' \item{\code{prica}}{print the constant adjustments values and changes
#' at each fit iteration.}
#' \item{\code{noprica}}{do not print the constant adjustments values and changes
#' at each fit iteration.}
#' \item{\code{prijac}}{print the fit Jacobian every time it is calculated.}
#' \item{\code{noprijac}}{do not print the fit Jacobian every time it is
#' calculated.}
#' \item{\code{supsot}}{to suppress all output of the normal solution process.}
#' \item{\code{nosupsot}}{to not suppress all output of the normal solution
#' process. Output will be a mess if this option is used.}
#' }
#' The default debug options are \code{c("noprica", "noprijac", "supsot")}
#'
#' @section SVD Analysis:
#'
#' If the inverse condition of the fit Jacobian is exactly zero,
#' then it is impossible to solve the equations of the fit procedure,
#' and the fit procedure is terminated. When the inverse condition
#' is small but non-zero, the solution is often inaccurate or even unstable.
#' In some cases the (near) singularity is caused by (almost) zero rows
#' or columns of the fit Jacobian.  It is also possible that
#' some rows or columns are linearly dependent. The example below shows
#' a case where the rows are dependent.
#'
#' The Singular Value Decomposition (SVD) (see the Wikipedia article
#' about SVD (\url{https://en.wikipedia.org/wiki/Singular_value_decomposition})
#' may help to find the linear dependent rows and columns. The SVD analysis can be
#' enabled by specifying argument `svdtest_tol` of `set_fit_options`.
#'
#' The output of the SVD analysis are the left and right singular vectors of
#' the Jacobian. A left singular vector is a linear combination of the rows
#' of the Jacobian that is almost zero. A right singular vector is a linear
#' combination of the columns that is almost zero. An example for the ISLM model
#' is shown below.
#'
#' First we create an Isis model and prepare fit data.
#' ```{r results = "hide"}
#' mdl <- islm_mdl("2020Q1")
#' y <- regts(985, start = "2020q1")
#' yd <- regts(800, start = "2020q1")
#' c <- regts(600, start = "2020q1")
#' fit <- cbind(y, yd, c)
#' mdl$set_fit(fit)
#' mdl$set_rms(c(c = 5.0, i = 21, md = 2))
#' ```
#'
#' So we have the following fit targets:
#' ```{r}
#' mdl$get_fit()
#' ````
#'
#' We specify fit options so that the SVD analysis is performed and the
#' fit Jacobian is printed.
#' ```{r}
#' mdl$set_fit_options(svdtest_tol = 1e-8, dbgopt = "prijac")
#' ```
#'
#' Next solve the model. Because y and yd are related according to `y = yd - t`
#' (`t` is also linearly related to `y`, so there is a linear relation between
#' `y` and `yd`),
#' the fit Jacobian contains dependent rows.
#' ```{r}
#' mdl$solve()
#' ```
#'
#' @examples
#'
#' mdl <- islm_mdl("2020Q1")
#'
#' # print constant adjustment and Jacobian  for each fit iteration
#' mdl$set_fit_options(zealous = TRUE, dbgopt = c("prica", "prijac"))
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

#' \code{\link{IsisMdl}} method: Sets the Fair-Taylor relaxation factors for
#' specific variables.
#' @name set_ftrelax
#' @aliases get_ftrelax
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} sets the
#' Fair-Taylor relaxation factors for specific endogenous leads.
#' By default, the Fair-Taylor relaxation factor used for all variables
#' is the value of solve option `xrelax`
#' (see \code{\link{set_solve_options}}),
#' which has the default value of 0.5. Method `set_ftrelax` can be used
#' to change the Fair-Taylor relaxation factor for specific variables.
#' If the relaxation factor for a specific variable is set to `NA`, then
#' the general Fair-Taylor relaxation factor `xrelax` will be used for
#' that variable.
#'
#' Method \code{get_ftrelax()} returns
#' the Fair-Taylor relaxation factors
#' for all endogenous leads.
#' @section Usage:
#' \preformatted{
#' mdl$set_ftrelax(value, pattern, names)
#'
#' mdl$get_ftrelax()
#'
#' # Clear all relaxation factors for specific variables
#' mdl$set_ftrelax(NA)
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{value}}{Fair-Taylor relaxation number.
#' This must be a positive number or \code{NA} to disable any previously set value.
#' The default value for all endogenous leads is \code{NA}, which means that
#' the general uniform Fair-Taylor relaxation
#' (solve option \code{ftrelax}, see \code{\link{set_solve_options}})
#' will be applied}
#' \item{\code{pattern}}{a regular expression specifying the
#' variable names}
#' \item{\code{names}}{a character vector with variable names}
#' }
#'
#' If neither \code{pattern} nor \code{names} have been specified,
#' then the Fair-Taylor relaxation factors of all variables
#' with endogenous leads will be set to the specified values.
#'
#' @examples
#' mdl <- ifn_mdl()
#'
#' # set Fair-Taylor relaxation factor all all variables with names of length 2
#' # to 0.5:
#' mdl$set_ftrelax(0.5, pattern = "^..$")
#'
#' # set Fair-Taylor relaxation factor for variable "lambda":
#' mdl$set_ftrelax(0.5, names = "lambda")
#'
#' print(mdl$get_ftrelax())
NULL

#' \code{\link{IsisMdl}} method: Set the values of model parameters
#' @name set_param
#' @aliases  set_param_values get_param
#'
#' @description
#' Method `set_param` and `set_param_values` of R6 class \code{\link{IsisMdl}}
#' can be used to set the values of model parameters.
#' A parameter may have more than one element. The parameter
#' value is a numeric vector of the appropriate length.
#'
#' Method `set_param` can be used to specify individual parameters,
#' while `set_param_values` is a convenient method to give more than one
#' parameter the same value.
#'
#' Method \code{get_param} returns a list with the
#' values of the parameters.
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_param(p, name_err = c("warn", "stop", "silent"))
#'
#' mdl$set_param_values(value, names, pattern)
#'
#' mdl$get_param(pattern, names)
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @section Arguments:
#' \describe{
#' \item{\code{p}}{a named character vector or list.
#' The names are the names of the parameter names.
#' `p` should be a list if one or more of the specified parameters are vector
#' parameters, i.e. parameters with a length greater than 1. In that
#' case, the list elements are numeric vectors with a length equal to the length
#' of the corresponding parameter.}
#' \item{\code{name_err}}{A character that specifies the
#' action that should be taken when a name is not the name of a parameter.
#' For `"warn"` (the default), a warning is given, for `"stop"` an error is
#' issued. For `"silent"`, the variable is silently skipped.}
#' \item{\code{value}}{A numeric vector of the appropriate length. All parameters
#' specified with argument `names` and `pattern` must have the same length as
#' argument `value`.}
#' \item{\code{names}}{A character vector specifying the names of the parameters.}
#' \item{\code{pattern}}{A regular expression. The action (get or
#' set parameter values) is applied to all parameters with names
#' matching \code{pattern}.}
#' }
#' If neither \code{names} nor \code{pattern} has been specified in
#' methods \code{set_param_values} or \code{get_param}, then the action
#' is applied to all model parameters.
#'
#' @examples
#' mdl <- islm_mdl()
#' mdl$set_param(list(i0 = 101))
#'
#' # give parameters i0, c0, m0, and t0 the value 0
#' mdl$set_param_values(0, pattern = ".0")
#'
#' # print all parameters
#' mdl$get_param()
#'
#' # print parameters c0, c1, c2 and c3
#' print(mdl$get_param(pattern = "^c.*"))
NULL

#' Writes an \code{IsisMdl} object to  a file
#' @name write_mdl
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' serializes the model object and writes it to a binary file.
#' The model can be read back by function
#' \code{\link{read_mdl}}.
#'
#' @details
#' \code{write_mdl} employs the serialization interface provided
#' by base R function \code{\link[base:readRDS]{saveRDS}}.
#'
#' @section Usage:
#' \preformatted{
#' mdl$write_mdl(file)
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @section Arguments:
#'
#' \describe{
#' \item{\code{file}}{the filename. Preferably use the extension
#' \code{.ismdl} so that it is obvious that the written file
#' contains a serialized \code{IsisMdl} object.}
#' }
#' @examples
#' mdl <- islm_mdl("2017Q1/2019Q2")
#' mdl$write_mdl("islm_mdl.ismdl")
#' @seealso \code{\link{read_mdl}}
NULL

#' Serializes the model to an \code{serialized_isismdl} S3 class
#' @name serialize
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' serializes the model object and returns
#' an \code{serialized_isismdl} object, an S3 object that contains
#' all the information about the model.
#' The serialized model can be used to create a new
#' \code{IsisMdl} object with the command
#' \code{IsisMdl$new(serialized_mdl)}
#'
#' @section Usage:
#' \preformatted{
#' mdl$serialize()
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @examples
#' mdl <- islm_mdl("2017Q1/2019Q2")
#' serialized_mdl <- mdl$serialize()
#'
#' # create a new model from the serialized model
#' mdl2 <- IsisMdl$new(serialized_mdl)
#' @seealso \code{\link{write_mdl}} and \code{\link{read_mdl}}
NULL


#' \code{\link{IsisMdl}} method: deletes all fit targets and rms values
#' values
#' @name clear_fit
#' @description
#' This methods of R6 class \code{\link{IsisMdl}}
#' deletes all fit targets and root mean square (rms) error values
#' for the fit procedure.
#'
#' @section Usage:
#' \preformatted{
#' mdl$clear_fit()
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @seealso \code{\link{set_fit}}, \code{\link{set_fit_values}}
#' and \code{\link{get_fit}}.
NULL

#' \code{\link{IsisMdl}} method: deletes all fix values
#' @name clear_fix
#' @description
#' This methods of R6 class \code{\link{IsisMdl}}
#' deletes all fix values specified with methods
#' \code{\link{set_fix}}, \code{\link{set_fix_values}} and
#' \code{\link{fix_variables}}
#'
#' @section Usage:
#' \preformatted{
#' mdl$clear_fix()
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @seealso \code{\link{set_fix}}, \code{\link{set_fix_values}},
#' \code{\link{fix_variables}} and \code{\link{get_fix}}
NULL

#' \code{\link{IsisMdl}} method: Returns a copy of this \code{IsisMdl} object
#' @name copy
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}}
#' returns a deep copy of an \code{IsisMdl} object
#' @section Usage:
#' \preformatted{
#' mdl$copy()
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Details:
#' \code{mdl$copy()} is  equivalent to \code{mdl$clone(deep = TRUE)}
#'
#' @examples
#' mdl <- islm_mdl("2017Q1/2019Q2")
#' mdl2 <- mdl$copy()
NULL

#' \code{\link{IsisMdl}} method: Fix variables to their current values
#' @name fix_variables
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} fixes the specified
#' frml variables to their current values in the model data.
#'
#' Each frml equation has a constant adjustment. If the frml equation is fixed,
#' then the left hand side of the equation (the  frml variable) is kept fixed
#' at the current value in the model data, and the
#' constant adjustment is calculated as the difference between the frml
#' variable and the right hand side of the equation.
#'
#' @section Usage:
#' \preformatted{
#' mdl$fix_variables(names, pattern, period = mdl$get_period())
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{pattern}}{a regular expression specifying the
#' variable names}
#' \item{\code{names}}{a character vector with variable names}
#' \item{\code{period}}{an \code{\link[regts]{period_range}} object or an
#' object that can be coerced to a \code{period_range}}
#' }
#' If neither \code{names} nor \code{pattern} has
#' been specified, then all frml variables are fixed.
#'
#' @examples
#' mdl <- islm_mdl("2015Q2/2016Q3")
#' mdl$solve()
#'
#' # fix variable "c" for a specific period:
#' mdl$fix_variables(names = "c", period = "2015Q3/2015Q4")
#'
#' # fix all frml variables
#' mdl$fix_variables(pattern = ".*")
#' @seealso \code{\link{set_fix}}, \code{\link{get_fix}}
#' and \code{\link{clear_fix}}.
NULL

#' \code{\link{IsisMdl}} method: R Sets the debug equation option
#' @name set_debug_eqn
#' @aliases get_debug_eqn
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} sets the
#' debug equation option (\code{TRUE} or \code{FALSE})
#'
#' Method \code{get_debug_eqn()} returns the debug equation option.
#' @section Usage:
#' \preformatted{
#' mdl$set_debug_eqn(value)
#'
#' mdl$get_debug_eqn()
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{value}}{A logical. If \code{TRUE}, then equation debugging
#' is turned on. The default is \code{FALSE}}
#' }
#'
#' @section Details:
#'
#' When a model cannot be solved this may be caused by errors in the
#' model equations or even errors in the initial data.
#' If debug mode is set to on, Isis will print messages in the
#' output file whenever it encounters numerical problems during calculation
#' of an equation.
#'
#' @seealso \code{\link{set_solve_options}}
#' @examples
#' mdl <- islm_mdl()
#' mdl$set_debug_eqn(TRUE)
#'
#' print(mdl$get_debug_eqn())
NULL

#' \code{\link{IsisMdl}} method: orders the equations of a model

#' @name order
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} orders the equations of a model.
#' This can be useful after (de)activation equations.
#' By specifying argument \code{orfnam} it is also possible to write ordering
#' information to a file.
#' @section Usage:
#' \preformatted{
#' mdl$order(orfnam, silent = FALSE)
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{orfnam}}{Name of file on which to print ordering information.
#'   If no output file is specified no ordering information will
#'   be written}
#' \item{\code{silent}}{A logical (default \code{FALSE}). If \code{TRUE}, then
#' output is suppressed.}
#' }
NULL

#' \code{\link{IsisMdl}} method: Returns the model text file

#' @name get_text
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the model text,
#' i.e. the contents of the model file passed to function \code{\link{isis_mdl}}.
#' \cr
#' In principle, it is possible to remove the original model file after
#' the \code{IsisMdl} object has been created and saved to a file with method
#' \code{\link{write_mdl}}, since the model text used to create the model
#' is stored in this file. However, this is not a good idea if the
#' model contains preprocessor directives
#' (\code{#include} or \code{#if}). The current version of \code{isismdl}
#' does not handle preprocessor directives yet, but in future
#' versions \code{isismdl} will store the preprocessed model text (the model
#' text obtained by evaluating the preprocessor directives). \emph{Therefore,
#' we recommend to always keep the original model file}.
#' @section Usage:
#' \preformatted{
#' mdl$get_text()
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @examples
#' mdl <- islm_mdl()
#' cat(mdl$get_text())
NULL


#' \code{\link{IsisMdl}} method: Returns the dependency structure of the model
#' variables

#' @name get_dep_struct
#'
#' @description
#' This method of the R6 class \code{\link{IsisMdl}} returns the dependency
#' structure of the model. It returns a data frame with three columns:
#'
#' * **lhs**: Left-hand side variables (the dependent variables)
#' * **rhs**: Right-hand side variables (the variables appearing in the equation
#' for the corresponding lhs)
#' * **lag**: The lags (and possibly leads) with which the rhs variables appear.
#'
#' If the argument `one_lag_per_row = TRUE`, then the output contains one row per
#' rhs-variable–lag combination (i.e., each lag gets its own row). Otherwise,
#' all lags for a given rhs variable appear in a single row.
#'
#'
#' For example, consider the equation:
#'
#' ```r
#' a = b - b[-1];
#' ```
#'
#' When `one_lag_per_row = FALSE` (the default), multiple lags for the
#' same rhs are separated by spaces and appear in one row:
#'
#' ```
#'   lhs   rhs   lag
#'   a     b     0 -1
#' ```
#'
#' When `one_lag_per_row = TRUE`, each lag appears on its own row:
#'
#' ```
#'   lhs   rhs   lag
#'   a     b     0
#'   a     b    -1
#' ```
#'
#' @section Usage:
#'
#' \preformatted{
#' mdl$get_dep_struct(one_lag_per_row = FALSE)
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{one_lag_per_row}}{A logical (default `FALSE`).
#' If `TRUE` for each rhs variable there is one row for each lag
#' that occurs in the equation.}
#' }
#' @examples
#' mdl <- islm_mdl()
#' print(mdl$get_dep_struct())
#' print(mdl$get_dep_struct(one_lag_per_row = TRUE))
#'
NULL

#' \code{\link{IsisMdl}} method: Returns the last solve period

#' @name get_last_solve_period
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the last solve period,
#' i.e. the last period for which method \code{\link{solve}} attempted to
#' find a solution (whether successful or not). The period is returned as a
#' \code{\link[regts]{period}} object. The function returns \code{NULL} when
#' method \code{solve} has not yet been used for this \code{IsisMdl} object.
#' @section Usage:
#' \preformatted{
#' mdl$get_last_solve_period()
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#' @examples
#' mdl <- islm_mdl(period = "2018q1/2018q4")$solve()
#' mdl$get_last_solve_period()
NULL


#' \code{\link{IsisMdl}} method: Set and get user data
#' @name set_user_data
#' @aliases get_user_data
#'
#' @description
#' An \code{\link{IsisMdl}} object is equipped with a list with user data.
#' This list is empty by default. With method \code{set_user_data} of
#' R6 class `IsisMdl` elements can be added to this list.
#' Method \code{get_user_data()} returns the user data.
#'
#' @section Usage:
#' \preformatted{
#' mdl$set_user_data(user_data, ...)
#'
#' mdl$get_user_data(key)
#'
#' }
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
#' @section Arguments:
#'
#' \describe{
#' \item{\code{user_data}}{The user data. This should be a named list.}
#'  \item{\code{...}}{The other arguments passed to \code{set_user_data} are
#'  used to update the user data list. See the examples.}
#' \item{\code{key}}{A character specifying the key(s) of the user data elements
#' to retrieve. If not specified the complete user data list is returned.}
#' }
#'
#' Function \code{get_user_data} returns a single element of the user data list
#' if argument \code{key} has been specified and if this is a single character;
#' otherwise the function returns a list.
#'
#' To remove an element of the list, set it to `NULL` (see example)
#'
#' @examples
#' mdl <- islm_mdl(period = "2021q1/2021q2")
#'
#' mdl$set_user_data(date = Sys.Date(),
#'                   note = "Example of user data")
#'
#' # the previous statement is equivalent to:
#' mdl$set_user_data(list(date = Sys.Date(),
#'                        note = "Example of user data"))
#'
#' # add another user data element
#' mdl$set_user_data(input_data = mdl$get_data())
#'
#' # print all user data
#' print(mdl$get_user_data())
#'
#' # print a specific element of the user data
#' print(mdl$get_user_data("date"))
#'
#' # print two specific elements of the user data
#' print(mdl$get_user_data(c("date", "input_data")))
#'
#' # remove user data element 'input_data':
#' mdl$set_user_data(input_data = NULL)
#' print(mdl$get_user_data())
#'
NULL


#' \code{\link{IsisMdl}} method: Returns the names of the simultaneous variables
#' @name get_simul_names
#'
#' @description
#' This method of R6 class \code{\link{IsisMdl}} returns the
#' names of the simultaneous variables in solution order.
#' The result never includes the left hand sides of inactive equations.
#' A simultanous variable is a variable that occurs in a feedback loop
#' and there directly or indirectly depends on itself.
#' @section Usage:
#' \preformatted{
#' mdl$get_simul_names()
#' }
#'
#' \code{mdl} is an \code{\link{IsisMdl}} object
#'
##' @examples
#' mdl <- islm_mdl()
#' mdl$get_simul_names()
NULL
