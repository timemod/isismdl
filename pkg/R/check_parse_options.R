check_parse_options <- function(parse_options_in) {

  default_parse_options <- list(flags = NULL, include_dirs = NULL)

  parse_options <- default_parse_options

  if (!missing(parse_options_in) && !is.null(parse_options_in)) {

    unknown_parse_options <- setdiff(names(parse_options_in),
                                     names(default_parse_options))
    if (length(unknown_parse_options) > 0) {
      stop("Unknown parse options ", paste(unknown_parse_options,
                                           collapse = ", "))
    }

    flags <- parse_options_in$flags
    if (!is.null(flags) && !is.character(flags)) {
      stop("Parse option flags should be a character vector")
    }
    include_dirs <- parse_options_in$include_dirs
    if (!is.null(include_dirs) && !is.character(include_dirs)) {
      stop("Parse option include_dirs should be a character vector")
    }

    names <- names(parse_options_in)
    parse_options[names] <- parse_options_in
  }

  return(parse_options)
}
