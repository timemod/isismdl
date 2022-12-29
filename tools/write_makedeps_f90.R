# This scripts creates a Makefile with dependencies
# of object files compiled using Fortran on .mod files.

library(igraph)
library(tictoc)

rm(list = ls())

source("tools/parameters.R")

deps <- readRDS(dep_f90_rds)
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


tic("writing dep_file")
con <- file(dep_f90_file, "wt")
for (src_name in names(deps)) {
  obj_file <- paste0(src_name, ".o")
  deps <- names(subcomponent(g, src_name, mode = "in")[-1])
  deps <- sort(deps)
  mod_files <- paste0(deps, ".mod")
  txt <- paste(obj_file, ":", paste(mod_files, collapse = " "))
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
