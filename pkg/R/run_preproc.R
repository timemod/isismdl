#' @useDynLib isismdl run_preproc_c
#' @export
run_preproc <- function(model_file, output_file, parse_options) {

  model_file <- check_mdl_file(model_file)
  parse_options <- check_parse_options(parse_options)

  flags <- parse_options$flags
  include_dirs <- parse_options$include_dirs

  return(.Call(run_preproc_c, model_file, output_file, flags, include_dirs))
}
