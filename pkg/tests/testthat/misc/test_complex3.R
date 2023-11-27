library(utils)
library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE


source("../tools/read_mrf.R")

period <- as.period_range("2017m12")
expected_output_file <- "expected_output/complex3.rds"
orf_name <- "output/complex3.orf"
expected_orf_name <- "expected_output/complex3.orf"
mdl_filename <- "mdl/complex3.mdl"
mdl_subst_filename <- "mdl/complex3_subst.mdl"

# create model
mdl <- isis_mdl(mdl_filename, period, silent = TRUE)
data_per <- mdl$get_data_period()
mdl$set_values(seq_len(nperiod(data_per)), names = paste0("X", 1:2))
mdl$set_solve_options(report = "none")

# convert model by using function substitution
outp <- capture.output(convert_mdl_file(mdl_filename, mdl_subst_filename,
                       conversion_options = list(substitute = TRUE)))

# create model with substituted user functions
outp <- capture.output(mdl_subst <- isis_mdl(mdl_subst_filename, period))
mdl_subst$set_values(seq_len(nperiod(data_per)), names = paste0("X", 1:2))
mdl_subst$set_solve_options(report = "none")

test_that("mdl_subst is correct", {
  expect_identical(mdl_subst$get_data_period(), data_per)
  expect_identical(mdl$get_var_names(), mdl_subst$get_var_names())
})

test_that("order works correctly", {

  mdl_old <- mdl$copy()
  outp  <- capture.output(mdl$order(orfnam = orf_name))
  expect_equal(mdl, mdl_old)

  # compare orf files
  orf <- readLines(orf_name)
  expected_orf <- readLines(expected_orf_name)
  expect_identical(orf, expected_orf)
})

test_that("model is solved correctly", {

  mdl$solve()
  expect_known_value(mdl$get_data(), file = expected_output_file,
		      update = update)

  mdl_subst$solve()
  expect_equal(mdl$get_data(), mdl_subst$get_data())
})

test_that("check mrf", {
  mrf_data <- read_mrf(mdl_filename)
  expect_known_output(cat(mrf_data), file = "expected_output/complex3_mrf.txt",
                      update = update, print = TRUE)

})

