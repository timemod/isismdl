library(utils)
library(isismdl)
library(testthat)

rm(list = ls())


mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

test_that("set_user_data/get_user data work correctly", {

  expect_equal(mdl$get_user_data(), list())

  mdl$set_user_data(list(x = 1:2, txt = "xxx"))

  mdl$set_user_data(user_data = list(x = 2:3, txt = "yyy"),
                    p = 2, q  = 5)

  mdl$set_user_data(z = TRUE, q = NULL)

  expected_result <- list(x = 2:3, txt = "yyy", p = 2, z = TRUE)
  expect_equal(mdl$get_user_data(), expected_result)
  expect_equal(mdl$get_user_data(c("p", "x")), expected_result[c("p", "x")])

  expect_error(mdl$get_user_data(c("p", "x", "zzz")),
               "The following keys are not present in user data:\n'zzz'")
  expect_error(mdl$get_user_data(c("p", "x", "zzz", "rrr")),
               "The following keys are not present in user data:\n'zzz', 'rrr'")

  expect_equal(mdl$get_user_data(character(0)), expected_result[integer(0)])

  mdl$set_user_data(p = NULL)
  expected_result <- list(x = 2:3, txt = "yyy", z = TRUE)
  expect_equal(mdl$get_user_data(), expected_result)

  # write model and read back again
  rds_file <- tempfile(pattern = "ismdl", fileext = ".rds")
  mdl$write_mdl(rds_file)
  expect_equal(mdl$get_user_data(), expected_result)
  mdl_read <- read_mdl(rds_file, silent = TRUE)
  expect_equal(mdl_read$get_user_data(), expected_result)

  mdl$set_user_data(list(xxx = 2))
  mdl$set_user_data(NULL)

  expect_equal(mdl$get_user_data(), list(xxx = 2))
})


test_that("errors", {
  expect_error(mdl$set_user_data(2),
                 "Argument 'user_data' must be a named list")

  expect_error(mdl$get_user_data("zzzz"), "'zzzz' is not a user data key")

  expect_error(mdl$get_user_data(2), "Argument 'key' must be a character")
})
