#' @importFrom methods setOldClass
setOldClass("regts")

# regperiod_range class already defined in class IsisMdl

#'  class IsisMdlS4
#'
#'  A simple S4 class of Isis models
#'
#'  Aap
#'
#' @slot model_index the model index
#' @importFrom methods setClass
IsisMdlS4 <- setClass(

    "IsisMdlS4",

    # Define the slots
    slots = c(
        control = "ANY",
        params = "list",
        inactive_eqs = "character",
        solve_opts = "list",
        fit_opts = "list",
        maxlag = "integer",
        maxlead = "integer",
        names = "character",
        ca_names = "character",
        period = "regperiod_range",
        data_period = "regperiod_range",
        data = "regts",
        ca = "ANY",
        fix = "ANY",
        fit = "ANY"
    )
)
