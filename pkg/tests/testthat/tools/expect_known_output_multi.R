#' Expect Known Output Multi
#'
#' This function compares the output of an expression against multiple potential
#' expected output files. This is useful when different operating systems or
#' environments produce slightly different but valid outputs.
#'
#' @param expr Expression to evaluate.
#' @param base_path Base path for the expected output files (without _OS.txt suffix).
#' @param update If TRUE, the test will not be performed and instead a new expected
#'               output file for the current platform will be created/updated.
#' @export
expect_known_output_multi <- function(expr, base_path, update = FALSE) {
  # Capture the actual output
  actual_output <- capture.output(expr)
  actual_text <- paste(actual_output, collapse = "\n")

  # Find all existing expected output files for this test
  dirname <- dirname(base_path)
  basename_part <- basename(base_path)
  
  # Pattern to match numeric suffixes like _1.txt, _2.txt, etc.
  pattern <- paste0("^", basename_part, "_(\\d+)\\.txt$")
  all_files <- list.files(dirname, full.names = FALSE)
  
  # Filter files and extract numbers
  matches <- regexec(pattern, all_files)
  match_list <- regmatches(all_files, matches)
  
  expected_files_info <- lapply(match_list, function(m) {
    if (length(m) == 2) {
      return(list(file = file.path(dirname, m[1]), num = as.integer(m[2])))
    }
    return(NULL)
  })
  expected_files_info <- expected_files_info[!vapply(expected_files_info, is.null, logical(1))]

  if (update) {
    # If update is TRUE, create/update the first variant or specific one? 
    # Usually update means overwrite existing if possible.
    # For simplicity, if no files exist, create _1.txt. If they exist, overwrite _1.txt?
    # Or based on user request "add one to the output generated" maybe update should also add new?
    # Typically expect_known_output(update=TRUE) overwrites. 
    # Let's check matches first.
  }

  # Check if actual output matches ANY of the expected files
  match_found <- FALSE
  for (info in expected_files_info) {
    expected_text <- paste(readLines(info$file, warn = FALSE), collapse = "\n")
    if (actual_text == expected_text) {
      match_found <- TRUE
      break
    }
  }

  if (match_found && !update) {
    expect_true(TRUE) # Pass the test
  } else {
    # No match found or we are forced to update
    
    # Determine the next number
    nums <- vapply(expected_files_info, function(x) x$num, integer(1))
    next_num <- if (length(nums) == 0) 1 else max(nums) + 1
    
    new_file_path <- sprintf("%s_%d.txt", base_path, next_num)
    writeLines(actual_text, new_file_path)

    if (update) {
       return(invisible(NULL))
    }

    # Fail the test with a descriptive message
    fail_msg <- sprintf(
      "Output does not match any existing expected output files.\nNew expected output file created: %s",
      new_file_path
    )
    fail(fail_msg)
  }

  return(invisible(NULL))
}
