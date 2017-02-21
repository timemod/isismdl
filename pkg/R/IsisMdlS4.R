#'  class IsisMdlS4
#'
#'  A simple S4 class of Isis models
#'
#'  Aap
#'
#' @slot model_index the model index
#' @export IsisMdlS4

#' @importFrom methods setClass
IsisMdlS4 <- setClass(

    "IsisMdlS4",

    # Define the slots
    slots = c(
        model_index = "integer",
        params = "list",
        inactive_eqs = "integer",
        maxlag = "integer",
        maxlead = "integer"
    ),

    # Set the default values for the slots. (optional)
    prototype=list(
        model_index = NA_integer_,
        params = list(),
        inactive_eqs = integer(0)
    )
)
