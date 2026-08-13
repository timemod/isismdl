#' Expect Known Output Multi
#'
#' This function compares the output of an expression against multiple potential
#' expected output files. This is useful when different operating systems or
#' environments produce slightly different but valid outputs.
#'
#' @param expr Expression to evaluate.
#' @param base_path Base path for the expected output files (without _X.txt suffix).
#' @param update If TRUE and no expected files exist yet, a new expected output
#'               file is created with a warning. If TRUE and existing files exist
#'               but none match, a new file is created and the test fails.
#' @noRd
expect_known_output_multi <- function(expr, base_path, update = FALSE) {
  actual_output <- capture.output(expr)
  actual_text <- paste(actual_output, collapse = "\n")

  dir <- dirname(base_path)
  base <- basename(base_path)
  pattern <- paste0("^", base, "_\\d+\\.txt$")
  expected_files <- list.files(dir, pattern = pattern, full.names = TRUE)



  # Check if actual output matches any of the expected files
  match_found <- any(vapply(expected_files, function(f) {
    identical(actual_text, paste(readLines(f, warn = FALSE), collapse = "\n"))
  }, logical(1)))

  if (match_found) {
    expect_true(TRUE)
    return(invisible(NULL))
  }

  # Geen match gevonden
  if (length(expected_files) > 0 && !update) {
    fail("Output does not match any existing expected output files.")
    return(invisible(NULL))
  }

  # update = TRUE: bepaal volgend nummer en schrijf nieuw bestand
  nums <- as.integer(regmatches(expected_files, regexpr("\\d+(?=\\.txt$)",
                                                        expected_files, perl = TRUE)))
  next_num <- if (length(nums) == 0) 1L else max(nums) + 1L
  new_file <- sprintf("%s_%d.txt", base_path, next_num)
  writeLines(actual_text, new_file)

  if (length(expected_files) == 0) {
    # Nog geen expected files: waarschuwing, geen failure
    warning(sprintf("No expected output files found. Created initial expected output file: %s",
                    new_file))
  } else {
    # Wel expected files maar geen match: nieuw bestand + failure
    fail(sprintf(
      paste0("Output does not match any existing expected output files.\n",
             "New expected output file created: %s"),
      new_file
    ))
  }

  return(invisible(NULL))
}
