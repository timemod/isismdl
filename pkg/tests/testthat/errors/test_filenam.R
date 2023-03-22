library(isismdl)
library(testthat)
library(readr)


rm(list = ls())

mdl_file <- "mdl/singular1.mdl"

test_that("file does not exist", {
  expect_error(isis_mdl(2),
               "Argument 'model_file' must be a character vector of length 1")
  expect_error(isis_mdl(rep(mdl_file, 2)),
               "Argument 'model_file' must be a character vector of length 1")
  expect_error(isis_mdl("aap.mdl", silent = TRUE),
               "The model file aap.mdl does not exist\n")
  expect_error(isis_mdl("mdl", silent = TRUE),
               "'mdl' is a directory")

  if (.Platform$OS.type == "unix") {
    mdl_file_abs <- normalizePath(mdl_file)
    # on Linux server cpb-rs-l02p normalizePath will append a /zfs
    home_dir <- normalizePath(Sys.getenv("HOME"))
    mdl_file_tilde <- sub(paste0("^", home_dir), "~", mdl_file_abs)
    expect_silent(isis_mdl(mdl_file_tilde, silent = TRUE))

    mdl_file_tilde2 <- sub("\\.mdl$", "", mdl_file_tilde)
    expect_silent(isis_mdl(mdl_file_tilde2, silent = TRUE))

    filename <- normalizePath(dirname(mdl_file_tilde))
    expect_error(isis_mdl(filename),
                 sprintf("'%s' is a directory", normalizePath(filename)))
  }

})
