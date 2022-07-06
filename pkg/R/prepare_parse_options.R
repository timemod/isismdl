prepare_parse_options <- function(parse_options_in) {

  default_parse_options <- list(flags = NULL, include_dirs = NULL,
                                gen_dep_file = FALSE)

  parse_options <- default_parse_options
  if (!missing(parse_options_in) && !is.null(parse_options_in)) {
    names <- names(parse_options_in)
    parse_options[names] <- parse_options_in
  }

  return(parse_options)
}
