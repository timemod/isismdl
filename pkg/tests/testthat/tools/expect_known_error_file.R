#' Check if the error file created by function isis_mdl() is equal to a
#' known result.
#'
#' The function reads the error file, removed CR on Windows and then
#' employs expect_known_output.
#'
#' @param err_file The filename of the .err file.
#' @param ... Arguments passed to `expect_known_output`.
#'
#' @returns
#' @export
#'
#' @examples
expect_known_error_file <- function(err_file, ...) {

  error_txt <- readr::read_file(err_file)

  # Remove Carriage Return on Windows
  if (.Platform$OS.type == "windows") {
    error_txt <- gsub("\r", "", error_txt)
  }

  expect_known_output(cat(error_txt), ...)
}
