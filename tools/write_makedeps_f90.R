# This scripts creates a Makefile with dependencies for the f90 files.
# An f90 file should be recompiled when imported module files (modules
# imported with the "use <module_name>" statement) are changed.
# The module files are created in directory mod when the corresponding
# f90 is is compiled AND if the interface of the module has been modified.
# So when the f90 file is compiled a new mod file is not always created.
# This makes it is a bit difficult to treat mod files in makefiles. Therefore
# we now recompile an f90 files if any object file corresponding to the
# imported modules have been modified.
library(tictoc)

rm(list = ls())

source("tools/parameters.R")

deps <- readRDS(dep_f90_rds)

tic("writing dep_file")
con <- file(dep_f90_file, "wt")
for (src_name in names(deps)) {
  obj_file <- sub(paste0("\\.", src_f90_ext, "$"), ".o", src_name)
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
