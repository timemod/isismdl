#!/usr/bin/Rscript
repo <- "https://cloud.r-project.org"

# install extra packages needed to install isismdl with the install script.
extra_packages <- c("devtools", "tictoc", "igraph")
for (extra_package in extra_packages) {
  if (!require(extra_package, character.only = TRUE, quietly = TRUE)) {
    install.packages(extra_package, repos = repo)
  }
}

devtools::install_deps("pkg", dependencies = TRUE, upgrade = "never",
                       repos = repo)
