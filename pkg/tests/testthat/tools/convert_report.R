# Replace CPU seconds with X in a solve report to make comparison with a
# reference report possible
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

cat_report <- function(report) {
  return(cat(paste(report, collapse = "\n")))
}
