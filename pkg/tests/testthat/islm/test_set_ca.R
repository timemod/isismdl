library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

context("set_ca for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))

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
  # create a data with duplicate names
  new_ca[, c("x1", "x2", "x3")] <- 999
  colnames(new_ca)[5:7] <- c("c", "md", "c")

  expect_warning(mdl2$set_ca(new_ca, upd_mode = "updval"),
                 paste("Data contains duplicate names. The first column is",
                       "used.\nThe duplicated names are: c, md\\."))
  expect_equal(mdl2$get_ca(), mdl$get_ca())
})



