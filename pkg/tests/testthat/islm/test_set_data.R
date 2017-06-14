library(utils)
library(isismdl)
library(testthat)

context("set_data for the  ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

new_data <-  mdl$get_data()
new_data["2015Q3", "g"] <- NA
new_data["2016Q2", "t"] <- NA

test_that("set_data update mode upd", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_data(new_data)
  expect_equal(mdl2$get_data(), new_data)

  mdl3 <- mdl$clone(deep = TRUE)
  mdl3$set_data(new_data, upd_mode = "upd")
  expect_equal(mdl2$get_data(), new_data)
})

test_that("set_data update mode updval", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_data(new_data, upd_mode = "updval")
  expect_equal(mdl2$get_data(), mdl$get_data())
})



