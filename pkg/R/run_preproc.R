#' @export
run_preproc <- function(model_file, parse_options) {


  #
  # check model file
  #
  if (!is.character(model_file) | length(model_file) > 1) {
    stop("Argument 'model_file' must be a character vector of length 1.")
  }
  if (dir.exists(model_file)) stop(sprintf("'%s' is a directory", model_file))
  if (.Platform$OS.type == "unix" && startsWith(model_file, "~")) {
    model_file <- sub("~", Sys.getenv("HOME"), model_file)
  }

  # TODO
  return(NULL)
}
