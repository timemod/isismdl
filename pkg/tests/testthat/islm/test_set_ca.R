library(utils)
library(isismdl)
library(testthat)

context("set_ca for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

new_ca <-  mdl$get_ca()
new_ca["2015Q3", "c"] <- NA
new_ca["2016Q2", "t"] <- NA

test_that("set_ca update mode upd", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_ca(new_ca)
  expect_equal(mdl2$get_ca(), new_ca)

  mdl3 <- mdl$clone(deep = TRUE)
  mdl3$set_ca(new_ca, upd_mode = "upd")
  expect_equal(mdl2$get_ca(), new_ca)
})

test_that("set_ca update mode updval", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_ca(new_ca, upd_mode = "updval")
  expect_equal(mdl2$get_ca(), mdl$get_ca())
})



