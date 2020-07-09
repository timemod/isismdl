# Replace CPU seconds with xxx in a solve report to make comparison with a
# reference report possible
convert_report <- function(report, replace_all_numbers = FALSE) {

  # Replace number in line "Solve model used XXX CPBU secs
  cpu_line_number <- grep("CPU secs$", report)
  cpu_line  <- report[cpu_line_number]
  report[cpu_line_number] <- sub("\\d+\\.\\d+", "xxx", report[cpu_line_number])

  report <- gsub("^(Total number of iterations\\s+)[1-9]\\d*\\s*", "\\1xxx",
                 report)

  report <- gsub("^(Convergence for \\d{4}Q\\d in\\s+)[1-9]\\d*\\s+iterations\\s*",
                 "\\1xxx ierations", report)

  num_pattern <- "(-|\\s)\\d*\\.\\d+([Ee][+-]?\\d+)?"
  if (replace_all_numbers) {
    report <- gsub(num_pattern, "xxx", report)
  } else {
    report <- sub(paste0("^(Singular value\\s+)", num_pattern), "\\1xxx",
                  report)
  }



  return(report)
}

cat_report <- function(report) {
  return(cat(paste(report, collapse = "\n")))
}
