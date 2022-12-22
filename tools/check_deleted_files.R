# Check if the dep_rds file contains files that do not longer
# exist. If so, then remove the depency files, so that they
# will be generated again
rm(list = ls())

source("tools/parameters.R")

if (file.exists(dep_rds)) {
  deps <- readRDS(dep_rds)
  files <- file.path(src_dir, names(deps))
  ok <- file.exists(files)
  if (any(!ok)) {
    file.remove(dep_rds)
    file.remove(dep_file)
  }
}