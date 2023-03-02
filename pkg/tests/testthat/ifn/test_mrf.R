library(isismdl)
library(testthat)
library(utils)

rm(list = ls())
update <- FALSE


source("../tools/read_mrf.R")

test_that("check mrf", {
  mdl_file <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  mdl_file_orig <- system.file("models", "ifn.mdl", package = "isismdl")
  file.copy(mdl_file_orig, mdl_file)
  mdl <- isis_mdl(mdl_file, silent = TRUE)
  mrf_data <- read_mrf(mdl_file)
  expect_known_output(cat(mrf_data),
                      file = "expected_output/mrf_mrf.txt",
                      update = update, print = TRUE)
})
