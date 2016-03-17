#' @useDynLib macromod read_mdl_c
#'
#' @export
read_mdl <- function(modelname) {

    model <- MacroModel$new()

    model$model_index <- .Call(read_mdl_c, modelname)

    return (model)
}
