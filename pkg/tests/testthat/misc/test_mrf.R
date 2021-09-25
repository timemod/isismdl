library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE

context("mrf")

source("../tools/read_mrf.R")

Sys.setlocale("LC_COLLATE", "C")

mdl_file <- "mdl/mrf_example1.mdl"
mdl <- isis_mdl(mdl_file, silent = TRUE)

mrf_data <- read_mrf(mdl_file)

test_that("check_mrf", {
  expect_known_output(cat(mrf_data), file = "expected_output/mrf_mrf.txt",
                    update = update, print = TRUE)
})
