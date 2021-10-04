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
  expect_known_output(cat(mrf_data),
                      file = "expected_output/mrf_example1_mrf.txt",
                      update = update, print = TRUE)
})

test_that("read old rds file", {
  expect_output(mdl_old <- read_mdl("old_rds_files/mrf_example1.rds"),
                "Model names will be reordered")
  expect_known_value(mdl_old$get_data(),
                     file = "expected_output/mrf_example1_mdl_old_data.rds")
  expect_equal(as.numeric(mdl_old$get_data(names = "g", period = 2020)), 39)
  expect_equal(as.numeric(mdl_old$get_data(names = "g_2", period = 2019)), 45)
  expect_equal(as.numeric(mdl_old$get_data(names = "g_22", period = 2020)), 53)
  expect_equal(as.numeric(mdl_old$get_data(names = "g4", period = 2017)), 57)
  expect_equal(as.numeric(mdl_old$get_data(names = "x", period = 2022)), 111)
  expect_equal(as.numeric(mdl_old$get_data(names = "X", period = 2022)), 118)
})
