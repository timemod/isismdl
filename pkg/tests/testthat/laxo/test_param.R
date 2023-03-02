library(testthat)
library(isismdl)


rm(list = ls())

update_expected <- FALSE

mdl_file <- "mdl/laxo.mdl"
mif_file <- "mdl/laxo.mif"

expect_silent(mdl <- isis_mdl("mdl/laxo.mdl", silent = TRUE))
param <- mdl$get_param()

test_that("get_param", {
  expect_known_output(param, print = TRUE, file = "expected_output/param.txt",
                      update_expected <- FALSE)
})

test_that("set_param", {
  mdl$set_param(list(omega = 1:180))
  expect_equal(mdl$get_param(names = "omega")[[1]], 1:180)
  expect_error(mdl$set_param(list(omega = 1)),
               "Value for parameter omega has an incorrect length. Required length: 180. Actual length: 1")
})

test_that("set_param_values", {
  expect_error(mdl$set_param_values(201:203, pattern = "^wg10?"),
               "Value for parameter wg10 has an incorrect length. Required length: 4. Actual length: 3")
  expect_equal(mdl$get_param(names = "wg1")[[1]], 201:203)
  expect_equal(mdl$get_param(names = "wg10")[[1]], param$wg10)
  expect_error(mdl$set_param_values(1),
               "Value for parameter omega has an incorrect length. Required length: 180. Actual length: 1")

})
