% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IsisMdl_doc.R
\name{set_param}
\alias{set_param}
\alias{set_param_values}
\alias{get_param}
\title{\code{\link{IsisMdl}} method: Set the values of model parameters}
\description{
Method \code{set_param} and \code{set_param_values} of R6 class \code{\link{IsisMdl}}
can be used to set the values of model parameters.
A parameter may have more than one element. The parameter
value is a numeric vector of the appropriate length.

Method \code{set_param} can be used to specify individual parameters,
while \code{set_param_values} is a convenient method to give more than one
parameter the same value.

Method \code{get_param} returns a list with the
values of the parameters.
}
\section{Usage}{

\preformatted{
mdl$set_param(p, name_err = c("warn", "stop", "silent"))

mdl$set_param_values(value, names, pattern)

mdl$get_param(pattern, names)

}
\code{mdl} is an \code{\link{IsisMdl}} object
}

\section{Arguments}{

\describe{
\item{\code{p}}{a named character vector or list.
The names are the names of the parameter names.
\code{p} should be a list if one or more of the specified parameters are vector
parameters, i.e. parameters with a length greater than 1. In that
case, the list elements are numeric vectors with a length equal to the length
of the corresponding parameter.}
\item{\code{name_err}}{A character that specifies the
action that should be taken when a name is not the name of a parameter.
For \code{"warn"} (the default), a warning is given, for \code{"stop"} an error is
issued. For \code{"silent"}, the variable is silently skipped.}
\item{\code{value}}{A numeric vector of the appropriate length. All parameters
specified with argument \code{names} and \code{pattern} must have the same length as
argument \code{value}.}
\item{\code{names}}{A character vector specifying the names of the parameters.}
\item{\code{pattern}}{A regular expression. The action (get or
set parameter values) is applied to all parameters with names
matching \code{pattern}.}
}
If neither \code{names} nor \code{pattern} has been specified in
methods \code{set_param_values} or \code{get_param}, then the action
is applied to all model parameters.
}

\examples{
mdl <- islm_mdl()
mdl$set_param(list(i0 = 101))

# give parameters i0, c0, m0, and t0 the value 0
mdl$set_param_values(0, pattern = ".0")

# print all parameters
mdl$get_param()

# print parameters c0, c1, c2 and c3
print(mdl$get_param(pattern = "^c.*"))
}
