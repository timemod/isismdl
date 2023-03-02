library(utils)
library(isismdl)
library(testthat)

rm(list = ls())
update_expected <- FALSE


source("../tools/convert_report.R")
source("../tools/read_mrf.R")


period <- period_range(2018, 2019)
mdl_file <- "mdl/missing_exo_lag1.mdl"
rep <- capture_output(mdl <- isis_mdl(mdl_file, period))

mdl$set_values(c(0, 1), names = "switch", period = period)
mdl$set_values(1, names = "z1", period = 2019)

y3_expected <- regts(c(3, 2), start = "2018")

test_that("standard option stop", {
  mdl2 <- mdl$copy()
  report <- capture.output(
  expect_warning(mdl2$solve(),
                 "Initial lags/leads missing/invalid. Simulation not possible."))
  expect_known_output(cat_report(report),
                      "expected_output/missing_exo_lag1_report1.txt",
                      update = update_expected)
})

test_that("option continue", {
  mdl2 <- mdl$copy()
  report <- capture.output(
   expect_warning(mdl2$solve(options = list(erropt = "cont")), NA))
  report <- convert_report(report)
   expect_known_output(cat(paste(report, collapse = "\n")),
                       "expected_output/missing_exo_lag1_report2.txt",
                       update = update_expected)
   y3_result <- mdl2$get_data(names = "y3", period = period)[ ,1]
   expect_identical(y3_result, y3_expected)
})

test_that("option silent", {
  mdl2 <- mdl$copy()
  report <- capture.output(
    expect_warning(mdl2$solve(options = list(erropt = "silent")), NA))
  report <- convert_report(report)
  expect_known_output(cat(paste(report, collapse = "\n")),
                      "expected_output/missing_exo_lag1_report3.txt",
                      update = update_expected)
  y3_result <- mdl2$get_data(names = "y3", period = period)[ ,1]
  expect_identical(y3_result, y3_expected)
})

test_that("option cont in combination with report = \"none\"", {
  mdl2 <- mdl$copy()
  expect_silent(mdl2$solve(options = list(erropt = "cont", report = "none")))
  y3_result <- mdl2$get_data(names = "y3", period = period)[ ,1]
  expect_identical(y3_result, y3_expected)
})

test_that("check mrf", {
  mrf_data <- read_mrf(mdl_file)
  expect_known_output(cat(mrf_data),
                      file = "expected_output/missing_exo_lag1_mrf.txt",
                      update = update_expected, print = TRUE)

})



#mdl2 <- mdl$copy()
#mdl2$solve(options = list(erropt = "cont"))
#print(mdl2$get_data())

#mdl2 <- mdl$copy()
#mdl2$solve(options = list(erropt = "silent"))
#print(mdl2$get_data())
