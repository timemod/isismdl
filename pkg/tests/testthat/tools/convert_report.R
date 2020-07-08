# Replace CPU seconds with xxx in a solve report to make comparison with a
# reference report possible
convert_report <- function(report, replace_all_numbers = FALSE) {

  # Replace number in line "Solve model used XXX CPBU secs
  cpu_line_number <- grep("CPU secs$", report)
  cpu_line  <- report[cpu_line_number]
  report[cpu_line_number] <- sub("\\d+\\.\\d+", "xxx", report[cpu_line_number])

  if (replace_all_numbers) {
    report <- gsub("-?\\d*\\.\\d+([Ee][+-]?\\d+)?", "xxx", report)
  }

  return(report)
}

cat_report <- function(report) {
  return(cat(paste(report, collapse = "\n")))
}
