library(utils)
library(isismdl)
library(testthat)

rm(list = ls())
update_expected <- FALSE

context("test for complex model 1")

source("../tools/read_mrf.R")

period <- as.period_range("2013")
include_dirs <- paste0("mdlincl", 1:2)
expected_output_file <- "expected_output/complex1.rds"
orf_name <- "output/complex1.orf"
orf_subst_name <- "output/complex1_subst.orf"
expected_orf_name <- "expected_output/complex1.orf"
expected_orf_subst_name <- "expected_output/complex1_subst.orf"
mdl_filename <- "mdl/complex1.mdl"
mdl_subst_filename <- "mdl/complex1_subst.mdl"

parse_options <- list(include_dirs= include_dirs)

# create model
mdl <- isis_mdl("mdl/complex1.mdl", period,
                parse_options = parse_options, silent = TRUE)
data_per <- mdl$get_data_period()
mdl$set_values(seq_len(nperiod(data_per)), names = paste0("x", 1:3))
mdl$set_solve_options(report = "none")

# create a model with user substitution
mdl_file_subst <- "mdl/"
#convert model by using function substitution
ok <- convert_mdl_file(mdl_filename, mdl_subst_filename,
                       conversion_options = list(substitute = TRUE),
                      parse_options = parse_options)
expect_true(ok)
mdl_subst <- isis_mdl(mdl_subst_filename, period, silent = TRUE)
mdl_subst$set_values(seq_len(nperiod(data_per)), names = paste0("x", 1:3))
mdl_subst$set_solve_options(report = "none")

test_that("get_dep_struct", {
  expect_known_output(mdl$get_dep_struct(),
                      file = "expected_output/complex1_mdl.dep",
                      update = update_expected, print = TRUE)
})

test_that("function substitution is correct", {
  expect_known_output(cat(paste(readLines(mdl_subst_filename), collapse = "\n")),
                      file = "expected_output/complex1_mdl_subst.mdl",
                      update = update_expected, print = TRUE)
  expect_identical(mdl_subst$get_data_period(), data_per)
  expect_identical(mdl$get_var_names(), mdl_subst$get_var_names())
  expect_identical(mdl$get_dep_struct(), mdl_subst$get_dep_struct())
})

test_that("order works correctly", {

  mdl_old <- mdl$copy()
  mdl$order(orfnam = orf_name, silent = TRUE)
  expect_equal(mdl, mdl_old)

  # compare orf files
  orf <- readLines(orf_name)
  expected_orf <- readLines(expected_orf_name)
  expect_identical(orf, expected_orf)

  mdl_subst$order(orfnam = orf_subst_name, silent = TRUE)

  # compare orf files
  orf_subst <- readLines(orf_subst_name)
  expected_orf_subst <- readLines(expected_orf_subst_name)
  expect_identical(orf_subst, expected_orf_subst)
})

test_that("model is solved correctly", {

  mdl$solve()
  expect_equal_to_reference(mdl$get_data(), file = expected_output_file)

  mdl_subst$solve()
  expect_equal(mdl$get_data(), mdl_subst$get_data())
})

test_that("check mrf", {
  mrf_data <- read_mrf(mdl_filename)
  expect_known_output(cat(mrf_data), file = "expected_output/complex1_mrf.txt",
                      update = update_expected, print = TRUE)

})

test_that("test get_text", {
  mdl_text <- mdl$get_text()
  expect_known_output(cat(mdl_text),
                      file = "expected_output/complex1_mdl_text.mdl",
                      update = update_expected, print = TRUE)
  mdl_tmp <- tempfile("isismdl_test_", fileext = ".mdl")
  writeLines(mdl_text, mdl_tmp)
  mdl_test <- isis_mdl(mdl_tmp, silent = TRUE)
  expect_identical(mdl$get_var_names(), mdl_test$get_var_names())
  expect_identical(mdl$get_dep_struct(), mdl_test$get_dep_struct())
  expect_identical(mdl$get_text(), mdl_text)
})

test_that("parse options errors", {
  expect_error(isis_mdl(mdl_filename, parse_options = c(parse_options,
                                                   gep_dep_file = TRUE)),
               "Unknown parse options gep_dep_file")
  expect_error(isis_mdl(mdl_filename, parse_options = list(flags = 2)),
               "Parse option flags should be a character vector")
  expect_error(isis_mdl(mdl_filename,
                        parse_options = list(include_dirs = FALSE)),
               "Parse option include_dirs should be a character vector")

})
