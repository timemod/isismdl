# Set up Repositories: CPB and CRAN
repos <- c(
  "https://cloud.r-project.org",           # CRAN
  "file:/usr/local/lib/R/cpblib/cpb_repo"  # CPB
)

# If the user library does not exist, then create it
# and add it to the library path.
user_lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib_dir)) {
  dir.create(user_lib_dir, recursive = TRUE)
  .libPaths(user_lib_dir_user)
}

cat("\nLibrary paths:\n")
print(.libPaths())

if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools", repos = repos)
}
if (!require(lintr, quietly = TRUE)) {
  install.packages("lintr", repos = repos)
}

# install and update all dependenencies
devtools::install_deps("pkg", dependencies = TRUE, repos = repos)

# make sure all packages (including devtools and lintr) are up-to-date.
update.packages(lib.loc = user_lib_dir, repos = repos, ask = FALSE)
