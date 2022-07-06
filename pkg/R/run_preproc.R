#' @useDynLib isismdl preproc_mdl_file_c
#' @export
run_preproc <- function(model_file, output_file, parse_options) {

  model_file <- check_mdl_file(model_file)
  parse_options <- prepare_parse_options(parse_options)

  with(parse_options, {
    retval <- .Call(preproc_mdl_file_c, model_file, output_file, flags,
                    include_dirs)
    return(retval)
  })
}
