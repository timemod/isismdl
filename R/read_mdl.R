#' Read the model from a mif file

#' @useDynLib macromod read_mdl_c
#' @useDynLib macromod get_max_lag_lead_fortran
#'
#' @param mif_name the name of the mif file
#' @return a \link{MacroModel} object
#' @examples
#' copy_example_mdl("islm")
#' compile_mdl("islm.mdl")
#' islm_model <- read_mdl("islm.mif")
#' \dontshow{
#' unlink("islm.*")
#' }
#' @export
read_mdl <- function(mif_name) {

    model <- MacroModel$new()

    model$model_index <- .Call(read_mdl_c, mif_name)

    # get maximum lag and lead
    ret <- .Fortran("get_max_lag_lead_fortran", model_index = model$model_index,
                  maxlag = as.integer(1), maxlead = as.integer(1))
    model$maxlag <- ret$maxlag
    model$maxlead <- ret$maxlead

    return (model)
}
