# This internal R6 class is used to make sure that the Fortran memory is
# cleaned up after a compiled model has been garbage collected
#' @importFrom R6 R6Class
ModelControl <- R6Class("ModelControl",
    public = list(
                 index = NULL,
                 initialize = function(model_index) {
                     self$index <- model_index
                     reg.finalizer(self, f = function(e) {
                         #cat(sprintf("Removing mdl %d\n", e$index))
                         .Call(C_remove_mws_c, model_index = e$index)
                     }, onexit = TRUE)
                 }
            )
)
