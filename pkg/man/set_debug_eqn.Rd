% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IsisMdl_doc.R
\name{set_debug_eqn}
\alias{set_debug_eqn}
\alias{get_debug_eqn}
\title{\code{\link{IsisMdl}} method: R Sets the debug equation option}
\description{
This method of R6 class \code{\link{IsisMdl}} sets the
debug equation option (\code{TRUE} or \code{FALSE})

Method \code{get_debug_eqn()} returns the debug equation option.
}
\section{Usage}{

\preformatted{
mdl$set_debug_eqn(value)

mdl$get_debug_eqn()

}
\code{mdl} is an \code{\link{IsisMdl}} object
}

\section{Arguments}{


\describe{
\item{\code{value}}{A logical. If \code{TRUE}, then equation debugging
is turned on. The default is \code{FALSE}}
}
}

\section{Details}{


When a model cannot be solved this may be caused by errors in the
model equations or even errors in the initial data.
If debug mode is set to on, Isis will print messages in the
output file whenever it encounters numerical problems during calculation
of an equation.
}

\examples{
mdl <- islm_mdl()
mdl$set_debug_eqn(TRUE)

print(mdl$get_debug_eqn())
}
\seealso{
\code{\link{set_solve_options}}
}
