#
# Generate makedeps file in directory pkg/src and pkg/src/macro.
#

library(igraph)
library(stringr)
library(tictoc)

rm(list = ls())

source("tools/parameters.R")
source("tools/create_makedeps_functions.R")

tic("Total time")

cat("===========================================================\n")
cat("Analyzing dependencies of c files on header files\n")
cat("===========================================================\n\n")

pattern <- paste0("\\.(",
                  paste(c(src_ext_c, header_ext_c), collapse = "|"),
                  ")$")
filenames <- list.files(src_dir, pattern = pattern, recursive = FALSE)

tic("Analyzing dependencies C code")
deps_c <- get_dep_list_c(filenames, src_dir = src_dir)
toc()

tic("Construct dependency graph for C code")
# build flat edge lists:
to <- rep(names(deps_c), lengths(deps_c))
from   <- unlist(deps_c, use.names = FALSE)
edges_df <- unique(data.frame(from = from, to = to, stringsAsFactors = FALSE))
g <- graph_from_data_frame(edges_df, directed = TRUE)
toc()

tic("Writing dep_file_c")

src_pattern <- paste0("\\.",  src_ext_c, "$")
src_files <- grep(src_pattern, names(deps_c), value = TRUE)
src_files <- sort(src_files)

write_dep_file(
  src_files = src_files,
  dep_file = dep_file_c,
  g = g,
  is_f90 = FALSE
)
toc()

cat("===========================================================\n\n")

cat("===========================================================\n")
cat("Analyzing dependencies of f90 files on modules\n")
cat("===========================================================\n\n")

pattern <- paste0("\\.", src_ext_f90, "$")
filenames <- list.files(src_dir, pattern = pattern, recursive = FALSE)

tic("Analyzing dependencies C code")
deps_f90 <- get_dep_list_f90(filenames, src_dir = src_dir,
                             src_ext = src_ext_f90)
toc()

tic("Construct dependency graph for C code")
# build flat edge lists:
to <- rep(names(deps_f90), lengths(deps_f90))
from   <- unlist(deps_f90, use.names = FALSE)
edges_df <- unique(data.frame(from = from, to = to, stringsAsFactors = FALSE))
g_f90 <- graph_from_data_frame(edges_df, directed = TRUE)
toc()

tic("Writing dep_file_f90")

src_pattern <- paste0("\\.",  src_ext_f90, "$")
src_files <- grep(src_pattern, names(deps_f90), value = TRUE)
src_files <- sort(src_files)

write_dep_file(
  src_files = src_files,
  dep_file = dep_file_f90,
  g = g_f90,
  is_f90 = TRUE
)
toc()

cat("===========================================================\n\n")

toc()
