# This scripts creates a dependency file for source codes in directory
# pkg/src.

library(tictoc)

rm(list = ls())

source("tools/parameters.R")

# read the text of a file into a single character string
read_file <- function(filename) {
  text <- readChar(filename, nchars = file.size(filename), useBytes = TRUE)
  Encoding(text) <- "latin1"
  return(text)
}

read_dep_file <- function(dep_file) {
  text <- read_file(dep_file)
  text <- gsub("\\s+\\\\\r?\n", "", text)
  text <- gsub("(\r?\n){2}", "\\1", text)
  statements <- strsplit(text, "\r?\n")[[1]]
  statements <- strsplit(statements, "\\s+:\\s+")
  obj_files <- sapply(statements, FUN = function(x) x[1])
  deps <- sapply(statements, FUN = function(x) x[2])
  deps <- strsplit(deps, "\\s+")
  deps <- lapply(deps, FUN = sort)
  names(deps) <- obj_files
  deps <- deps[sort(obj_files)]
  return(deps)
}

deps <- read_dep_file(dep_file)
deps_old <- read_dep_file("pkg/src/deps/makedeps_old")

# missing elements:
print(setdiff(names(deps_old), names(deps)))
print(setdiff(names(deps), names(deps_old)))

for (name in names(deps)) {
  dep <- deps[[name]]
  dep_old <- deps_old[[name]]
  if (!identical(dep, dep_old)) {
    cat("\nverschillen gevonden voor", name, "\n")
    cat("extra in de nieuwe deps\n")
    print(setdiff(dep, dep_old))
    cat("verdwenen in de oude deps\n")
    print(setdiff(dep_old, dep))
    cat("\n\n")
  }
}
