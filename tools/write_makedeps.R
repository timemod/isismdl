# This scripts creates a Makefile with dependencies of source files in 
# directory pkg/src on the header files in directory pkg/src and 
# pkg/src/macro.

library(igraph)
library(tictoc)

rm(list = ls())

source("tools/parameters.R")

deps <- readRDS(dep_rds)

tic("construct matrix")
all_names <- sort(union(names(deps), unique(unlist(deps))))
n <- length(all_names)
dep_mat <- matrix(0, nrow = n, ncol = n,
                   dimnames = list(all_names, all_names))
for (naam in names(deps)) {
  for (dep in deps[[naam]]) {
    dep_mat[naam, dep] <- 1
  }
}
toc()

tic("create graph")
g <- graph_from_adjacency_matrix(t(dep_mat))
toc()

# TODO: give a warning about include files  that are not used

tic("writing dep_file")
src_pattern <- paste0("\\.(",  paste(src_ext, collapse = "|"), ")$")
src_files <- grep(src_pattern, names(deps), value = TRUE)
src_files <- sort(src_files)
con <- file(dep_file, "wt")
for (src_file in src_files) {
  obj_file <- sub(src_pattern, ".o", src_file)
  deps <- names(subcomponent(g, src_file, mode = "in")[-1])
  deps <- sort(deps)
  txt <- paste(obj_file, ":", paste(deps, collapse = " "))
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
