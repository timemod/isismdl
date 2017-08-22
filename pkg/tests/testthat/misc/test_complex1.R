library(utils)
library(isismdl)
library(testthat)

context("test for complex model 1")

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
capture.output(mdl <- isis_mdl("mdl/complex1.mdl", period,
                               parse_options = parse_options))
data_per <- mdl$get_data_period()
mdl$set_values(seq_len(nperiod(data_per)), names = paste0("x", 1:3))
mdl$set_solve_options(report = "none")

# convert model by using function substitution
capture.output(convert_mdl_file(mdl_filename, mdl_subst_filename,
                 conversion_options = list(substitute = TRUE),
                 parse_options = parse_options))

# create model with substituted user functions
capture.output(mdl_subst <- isis_mdl(mdl_subst_filename, period))
mdl_subst$set_values(seq_len(nperiod(data_per)), names = paste0("x", 1:3))
mdl_subst$set_solve_options(report = "none")

test_that("mdl_subst is correct", {
  expect_identical(mdl_subst$get_data_period(), data_per)
  expect_identical(mdl$get_var_names(), mdl_subst$get_var_names())
})

test_that("order works correctly", {

  mdl_old <- mdl$copy()
  capture.output(mdl$order(orfnam = orf_name))
  expect_equal(mdl, mdl_old)

  # compare orf files
  orf <- readLines(orf_name)
  expected_orf <- readLines(expected_orf_name)
  expect_identical(orf, expected_orf)

  capture.output(mdl_subst$order(orfnam = orf_subst_name))

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

