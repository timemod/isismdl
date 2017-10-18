# Replace CPU seconds with xxx in a solve report to make comparison with a
# reference report possible
convert_report <- function(report) {
  cpu_line_number <- grep("CPU secs$", report)
  cpu_line  <- report[cpu_line_number]
  report[cpu_line_number] <- gsub("\\d+\\.\\d+", "xxx",
                                  report[cpu_line_number])
  return(report)
}
