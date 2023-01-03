# Check if the dep_rds file contains files that do not longer
# exist. If so, then remove the dependency files, so that they
# will be generated again
rm(list = ls())

source("tools/parameters.R")

check_dep_rds <- function(dep_rds, dep_file, src_dir) {
  if (file.exists(dep_rds)) {
    deps <- readRDS(dep_rds)
    files <- file.path(src_dir, names(deps))
    ok <- file.exists(files)
    if (any(!ok)) {
      cat("\nRemoving file ", dep_rds, "\n\n")
      file.remove(dep_rds)
      file.remove(dep_file)
    }
  }
  return(invisible())
}

check_dep_rds(dep_f90_rds, dep_file = dep_f90_file, src_dir = src_dir)
check_dep_rds(dep_c_rds, dep_file = dep_c_file, src_dir = src_dir)

