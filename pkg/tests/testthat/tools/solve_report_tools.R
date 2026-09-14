#
# Functions for converting and checking isismdl solve reports.
#


#' Convert an isismdl solve report
#'
#' Replace the number of iterations and CPU seconds with X in a solve report,
#  to make comparison with a reference report possible. The report is typically
#' the output of function `capture.output`.
#'
#' @param report A character vector with one element for each line of the
#' report.
#' @param replace_all_numbers Logical (default `TRUE`): should all floating
#' point numbers in the report be replaced by whitespace characters?
#'
#' @returns A character vector with one element for each line of the
#' report.
#' @noRd
convert_report <- function(report, replace_all_numbers = FALSE) {

  report <- gsub("^(Total number of iterations)\\s+[1-9]\\d*\\s*", "\\1 X",
                 report)
  report <- gsub("^(Convergence for \\d{4}Q\\d in)\\s+[1-9]\\d*\\s+(iterations)\\s*",
                 "\\1 X iterations", report)

  num_pattern1 <- "-?\\d*\\.\\d+([Ee][+-]?\\d+)?"
  num_pattern2 <- "-?\\d+\\.\\d*([Ee][+-]?\\d+)?"

  report <- gsub(paste0("^(Solve model used)\\s+", num_pattern1, "\\s+(CPU secs)$"),
                 "\\1 X CPU secs", report)
  report <- gsub(paste0("^(Solve model used)\\s+", num_pattern2, "\\s+(CPU secs)$"),
                 "\\1 X CPU secs", report)

  report <- stringr::str_replace(report,
                                 paste0("^(Singular value)\\s+", num_pattern1, "\\s*$"),
                                 "\\1 X")
  report <- stringr::str_replace(report,
                                 paste0("^(Singular value)\\s+", num_pattern2, "\\s*$"),
                                 "\\1 X")


  repl_number <- function(x) {
    # Replace the number string with an empty string with the same number
    # of characters as X. Do not use something with "X", because of complications
    # due to alignment issues when negative numbers are printed or when some
    # numbers are printed with scientific format.
    return(stringr::str_pad("", nchar(x)))
  }


  if (replace_all_numbers) {
    report <- stringr::str_replace_all(report, num_pattern1, repl_number)
    report <- stringr::str_replace_all(report, num_pattern2, repl_number)
  }

  # remove trailing white space
  report <- trimws(report, which = "right")

  return(report)
}

#' Expect Known Solve Report Multi
#'
#' This function compares an isismdl solve report on a expected
#' expected output files. It first converts the report with function
#' convert_report and then employs expect_known_output_multi.
#'
#' @param report A character vector with the report as returned by
#' function `capture.output()`.
#' @param ... Arguments passed to `expect_known_output()`.
#' @param replace_all_numbers Logical (default `TRUE`): should all floating
#' point numbers in the report be replaced by whitespace characters?
#' @noRd
expect_known_report <- function(report, ...,
                                replace_all_numbers = FALSE) {

  report <- convert_report(report, replace_all_numbers = replace_all_numbers)

  report <- paste(report, collapse = "\n")

  expect_known_output(cat(report), ...)
}


#' Expect Known Solve Report Multi
#'
#' This function compares an isismdl solve report with multiple potential
#' expected output files. It first converts the report with function
#' convert_report and then employs expect_known_output_multi.
#' This is useful when different operating systems or
#' environments produce slightly different but valid outputs.
#'
#' @param report A character vector with the report as returned by
#' function `capture.output()`.
#' @param ... Arguments passed to `expect_known_output_multi`.
#' @param replace_all_numbers Logical (default `TRUE`): should all floating
#' point numbers in the report be replaced by whitespace characters?
#' @noRd
expect_known_report_multi <- function(report, ...,
                                      replace_all_numbers = FALSE) {

  report <- convert_report(report, replace_all_numbers = replace_all_numbers)

  report <- paste(report, collapse = "\n")

  expect_known_output_multi(
    cat(report),
    ...
  )
}
