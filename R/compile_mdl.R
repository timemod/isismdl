#' Compile a model file
#'
#' This function compiles a model file
#' @param modelname The name of the model file (including extension)
#' @return an integer return code (0 on success)
#' @useDynLib macromod compile_mdl_c
#' @export
compile_mdl <- function(modelname) {
    retval <- .Call(compile_mdl_c, modelname)
    return (retval)
}
