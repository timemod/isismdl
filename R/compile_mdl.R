#' @useDynLib macromod compile_mdl_c
#'
#' @export
compile_mdl <- function(modelname) {
    retval <- .Call(compile_mdl_c, modelname)
    return (retval)
}
