#
# This script checks the dependencies of f90 files on mod files.
# A dependency list is written file deps/deps_f90.rds.
library(stringr)
library(tictoc)

rm(list = ls())

source("tools/parameters.R")

read_used_modules <- function(filename, src_dir, is_macro_dir = FALSE) {
  src_name <- sub(paste0("\\.", src_f90_ext, "$"), "", filename)
  use_pattern = "^\\s*use\\s+([a-z][a-z0-9_]*)"
  lines <- readLines(file.path(src_dir, filename))
  modules <- character(0)
  for (line in lines) {
    ma <- str_match(line, use_pattern)
    module <- ma[, 2]
    if (!is.na(module)) {
      if (file.exists(file.path(src_dir, paste0(module, ".", src_f90_ext)))) {
        if (module == src_name) {
          stop("f90 file imports its own module for ", filename)
        }
        modules <- union(modules, module)
      }
    }
  }
  if (length(modules) == 0) {
    return(NULL)
  } else {
    return(modules)
  }
  return(modules)
}

get_dep_list <- function(filenames, src_dir, is_macro_dir = FALSE) {
  retval <- lapply(filenames, FUN = read_used_modules,
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

if (interactive() || !file.exists(dep_f90_rds)) {

  #
  # This script is run from RStudio or the dep_rds file
  # does not exist.
  #

  pattern <- paste0("\\.(", src_f90_ext, ")$")
  filenames <- list.files(src_dir, pattern = pattern, recursive = TRUE)

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

  deps <- readRDS(dep_f90_rds)
}

tic("Analyzing dependencies")
deps <- update_deps(deps, filenames, src_dir = src_dir)
toc()

saveRDS(deps, dep_f90_rds)
