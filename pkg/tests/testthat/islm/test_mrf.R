library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

update_expected  <- FALSE

context("mrf islm model")

source("../tools/read_mrf.R")

test_that("new compiled moel", {
  mdl_filename <- "mdl/islm.mdl"
  mdl <- isis_mdl(mdl_filename, silent = TRUE)
  mrf_data <- read_mrf(mdl_filename)
  expect_known_output(cat(mrf_data),
                      file = "expected_output/mrf_mrf.txt",
                      update = update_expected, print = TRUE)
})
