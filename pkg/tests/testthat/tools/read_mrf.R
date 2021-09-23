read_mrf <- function(mdl_file) {
  mrf_file <- paste0(tools::file_path_sans_ext(mdl_file), ".mrf")
  lines <- readLines(mrf_file)
  # remove the date from the first line, so that we can compare the
  # mrf file:
  lines[1] <- sub("(.+?)\\d.+", "\\1", lines[1])
  return(paste(lines, collapse = "\n"))
}


