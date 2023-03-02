library(utils)
library(isismdl)
library(testthat)

rm(list = ls())


mdl_filename <- "mdl/islm.mdl"
mdl <- isis_mdl(mdl_filename, period = "2018/2019", silent = TRUE)
rds_file <- tempfile(pattern = "isismdl", fileext = ".rds")
mdl$write_mdl(rds_file)

test_that("rds file read correcly", {
  mdl2 <- read_mdl(rds_file, silent = TRUE)
  unlink(rds_file)
  expect_equal(mdl, mdl2)
  expect_equal(mdl$get_data_period(), mdl2$get_data_period())
})
