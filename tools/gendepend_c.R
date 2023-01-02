#
# This script is used to check the dependencies for source files in
# directory pkg/src on the include files in pkg/src and pkg/src/macro.
# A dependency list is written file deps/deps.rds.

library(igraph)
library(stringr)
library(tictoc)

rm(list = ls())

source("tools/parameters.R")

read_includes <- function(filename, src_dir, is_macro_dir = FALSE) {
  include_pattern <- "^#\\s*include\\s+\"(.+)\""
  lines <- readLines(file.path(src_dir, filename))
  includes <- character(0)
  for (line in lines) {
    ma <- str_match(line, include_pattern)
    include <- ma[, 2]
    if (!is.na(include)) {
      if (is_macro_dir) include <- paste0("macro/", include)
      if (file.exists(file.path(src_dir, include))) {
        includes <- c(includes, include)
      }
    }
  }
  if (length(includes) == 0) {
    return(NULL)
  } else {
    return(includes)
  }
  return(includes)
}

get_dep_list <- function(filenames, src_dir, is_macro_dir = FALSE) {
  retval <- lapply(filenames, FUN = read_includes,
                   src_dir = src_dir, is_macro_dir = is_macro_dir)
  names(retval) <- filenames
  return(retval)
}

update_deps <- function(deps, filenames, src_dir) {

  if (length(filenames) > 0) {
    new_deps <- get_dep_list(filenames, src_dir = src_dir)
    deps[names(new_deps)] <- new_deps
  }


  # remove unnessary dependencies
  is_null <- sapply(deps, FUN = is.null)
  return(deps[!is_null])
}

if (interactive() || !file.exists(dep_c_rds)) {

  #
  # This script is run from RStudio or the dep_rds file
  # does not exist.
  #

  pattern <- paste0("\\.(",
                    paste(c(src_c_ext, c_header_ext), collapse = "|"),
                    ")$")
  filenames <- list.files(src_dir, pattern = pattern)

  cat("\nAnalyzing dependencies of c++ files on header files\n\n")

  deps <- list()

} else {

  # This script is called from the makefile Makedeps,
  # The command line arguments are the names of the files
  # that are newer than the dep_rds file.

  filenames <- commandArgs(trailingOnly = TRUE)
  pattern <- paste0("^", src_dir, "/")
  pattern <- sub("/", "(/|\\\\\\\\)", pattern)
  filenames <- sub(pattern, "", filenames)

  cat("\nUpdating dependencies of C++ files on header files:\n",
      paste(filenames, collapse = "\n"), "\n\n")

  deps <- readRDS(dep_c_rds)
}

tic("Analyzing dependencies")
deps <- update_deps(deps, filenames, src_dir = src_dir)
toc()

saveRDS(deps, dep_c_rds)
