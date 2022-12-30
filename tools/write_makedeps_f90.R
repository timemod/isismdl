# This scripts creates a Makefile with dependencies for f90 files on other
# f90 files because of the 'use' statement.
# The f90 files actually depends of the .mod files in
# directory pkg/src/mod, but since these mod files are created when
# f90 files are compiled, we can use the dependencies of mod files.

library(igraph)
library(tictoc)

rm(list = ls())

source("tools/parameters.R")

deps <- readRDS(dep_f90_rds)

tic("writing dep_file")
con <- file(dep_f90_file, "wt")
for (src_name in names(deps)) {
  obj_file <- paste0(src_name, ".o")
  dep_files <- deps[[src_name]]
  dep_files <-sort(dep_files)
  dep_files <- paste0(dep_files, ".o")
  txt <- paste(obj_file, ":", paste(dep_files, collapse = " "))
  lines <- strwrap(txt, width = 80, exdent = 4)
  nlines <- length(lines)
  if (nlines > 1) {
    nrs <- 1 : (nlines - 1)
    lines[nrs] <- paste(lines[nrs], "\\")
  }
  writeLines(c(lines, ""), con = con)
}
close(con)
toc()
