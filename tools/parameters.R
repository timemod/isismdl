#
# This script is used to check the dependencies for source files in
# directory pkg/src on the include files in pkg/src and pkg/src/macro.
# A dependency list is written file deps/deps.rds.

src_dir <- "pkg/src"
dep_f90_rds <- "deps/deps_f90.rds"
dep_c_rds <- "deps/deps_c.rds"
dep_f90_file <- "pkg/src/deps/makedeps_f90"
dep_f90_file <- "pkg/src/deps/makedeps_c"
src_f90_ext <- "f90" # possible extensions for source files
src_c_ext <- "c" # possible extensions for source files
c_header_ext <- "h" # possible extensions for header files
