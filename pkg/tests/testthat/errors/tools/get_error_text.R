#' Returns the text of the error file with syntax errors
#'
#' @param model_text Character: model text.
#' @param flags Compiler flags.
#' @return Character: contents of the .err file created by isismdl.
#' @noRd
get_error_text <- function(model_text, flags = character()) {

  msg <- "Error detected in compilation of model .+\\.mdl\nCheck the .err file"

  mdl_file <- tempfile("isismdl_test_", fileext = ".mdl")
  if (.Platform$OS.type == "windows") {
    mdl_file <- normalizePath(mdl_file, winslash = "/", mustWork = FALSE)
  }
  err_file <- sub("\\.mdl$", ".err", mdl_file)

  on.exit(
    file.remove(c(mdl_file, err_file))
  )

  writeLines(model_text, mdl_file)
  expect_error(
    isis_mdl(mdl_file, silent = TRUE, parse_options = list(flags = flags)),
    msg
  )
  if (file.exists(err_file)) {
    lines <- readLines(err_file)
    lines <- sub(mdl_file, "model_text", lines)
    return(paste(lines, collapse = "\n"))
  } else {
    return(NULL)
  }
}
