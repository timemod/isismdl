library(utils)
library(isismdl)
library(testthat)

context("solve ISLM model")

capture_output(mdl <- read_mdl("islm_model.rds"))
capture_output(mdl_solved <- read_mdl("islm_model_solved.rds"))

test_that("mdl is not equal to mdl_solved", {
  expect_false(isTRUE(all.equal(mdl, mdl_solved)))
})

report <- capture_output(mdl$solve())

isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)

dif <- tsdif(mdl$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)

test_that("Comparing ordinary solve for the ISLM model", {
  expect_true(all.equal(mdl, mdl_solved))
  expect_identical(mdl$get_data_period(),
                   period_range("2015Q1", "2016Q3"))
  expect_identical(mdl$get_var_names(), colnames(isis_result))
  expect_identical(mdl$get_endo_names(type = "frml"), c("c", "i", "md", "t"))
  expect_identical(dif$missing_names1, character(0))
  expect_identical(dif$missing_names2, character(0))
  expect_identical(dif$difnames, character(0))
})
