library(testthat)
library(isismdl)

context("fit for ISLM model")

capture_output(mdl <- read_mdl("islm_model.rds"))
all_eqs <- mdl$get_eq_names()

test_that("Testing equation status after clone", {
  expect_identical(mdl$get_eq_names(type = "inactive"), character(0))
  mdl2 <<- mdl$clone(deep = TRUE)
  mdl2$set_eq_status("inactive", names = "c")
  expect_identical(mdl$get_eq_names(type = "inactive"), character(0))
  expect_identical(mdl2$get_eq_names(type = "inactive"), "c")
  expect_identical(mdl2$get_eq_names(type = "active"), setdiff(all_eqs, "c"))
})

test_that("Testing equation status after reading the model", {
  mdl2$write_mdl("temp.rds")
  capture.output(mdl3 <- read_mdl("temp.rds"))
  unlink("temp.rds")
  expect_identical(mdl3$get_eq_names(type = "inactive"), "c")
})


