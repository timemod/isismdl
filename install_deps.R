#!/usr/bin/Rscript
# Set up Repositories: CPB and CRAN
repos <- c(
  CRAN = "https://cloud.r-project.org",
  CPB = "http://gitlab.cpb.nl:8880/io/cpb_repo/"
)

options(repos = repos)

# If the user library does not exist, then create it
# and add it to the library path.
user_lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib_dir)) {
  dir.create(user_lib_dir, recursive = TRUE)
  .libPaths(user_lib_dir)
}

# Install 'pak' if needed
if (!require("pak", character.only = TRUE, quietly = TRUE)) {
  install.packages("pak", dependencies = FALSE)
} else {
  unloadNamespace("pak")
}

# Install extra packages
extra_packages <- c("devtools", "tictoc", "lintr")
for (pkg in extra_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    pak::pkg_install(pkg, lib = .libPaths(), upgrade = FALSE)
  } else {
    unloadNamespace(pkg)
  }
}

# Install and update dependencies
pak::local_install_dev_deps("pkg", lib = .libPaths(), upgrade = FALSE)
