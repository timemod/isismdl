library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

context("read_mdl_dataperiod")

expect_silent(mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE))

data_solved <- mdl$get_data()

i <- regts(200, start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fit <- cbind(y, i)

c <- regts(c(600, NA, 600), start = '2015Q2')
md <- regts(205, start = '2015Q2')
fix <- cbind(c, md)

lbls <- mdl$get_labels()
fix_lbls <- update_ts_labels(fix, lbls)
fit_lbls <- update_ts_labels(fit, lbls)

test_that("set_fit", {
  mdl$set_fit(fit)
  # set_fit should not change the model data
  expect_equal(mdl$get_data(), data_solved)
})

test_that("set_fix", {
  mdl$set_fix(fix)
  expected_data <- update_ts(data_solved, fix, method = "updval")
  expect_equal(mdl$get_data(), expected_data)
})

test_that("write and read model", {
  mdl$set_data(data_solved)
  rds_file <- tempfile(pattern = "isismdl", fileext = ".rds")
  mdl$write_mdl(rds_file)
  expect_silent(mdl2 <- read_mdl(rds_file, silent = TRUE))
  ok <- file.remove(rds_file)
  expect_equal(mdl2$get_fix(), fix_lbls)
  expect_equal(mdl2$get_fit(), fit_lbls[, c("i", "y")])
  expect_equal(mdl2$get_data(), data_solved)
})
