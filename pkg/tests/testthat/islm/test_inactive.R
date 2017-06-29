library(testthat)
library(isismdl)

context("fit for ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))
all_eqs <- mdl$get_eq_names()
frml_names <- c("c", "i", "md", "t")

test_that("Testing equation status after clone", {
  expect_identical(mdl$get_eq_names(status = "inactive"), character(0))
  mdl2 <<- mdl$clone(deep = TRUE)
  mdl2$set_eq_status("inactive", names = "c")
  expect_identical(mdl$get_eq_names(status = "inactive"), character(0))
  expect_identical(mdl2$get_eq_names(status = "inactive"), "c")
  expect_identical(mdl2$get_eq_names(status = "active"), setdiff(all_eqs, "c"))
  expect_identical(mdl2$get_endo_names(type = "frml", status = "all"),
                   frml_names)
  expect_identical(mdl2$get_endo_names(type = "frml"), setdiff(frml_names, "c"))
})

test_that("pattern argument works correctly", {
  mdl2 <<- mdl$clone(deep = TRUE)
  mdl2$set_eq_status("inactive")
  expect_identical(mdl2$get_eq_names(status = "inactive"), all_eqs)
  mdl2$set_eq_status("active", pattern = "^y")
  expect_identical(mdl2$get_eq_names(status = "active"), c("y", "yd"))
  expect_identical(mdl2$get_eq_names(status = "inactive"),
                                    setdiff(all_eqs, c("y", "yd")))
})

test_that("Testing equation status after reading the model", {
  mdl2 <<- mdl$clone(deep = TRUE)
  mdl2$set_eq_status("inactive", names = "c")
  mdl2$write_mdl("temp.rds")
  capture.output(mdl3 <- read_mdl("temp.rds"))
  unlink("temp.rds")
  expect_identical(mdl3$get_eq_names(status = "inactive"), "c")
})

test_that("behaviour of deactivated equations", {
  mdl2 <- mdl$copy()
  mdl2$set_eq_status("inactive", names = "c")
  mdl2$set_values(600, names = "c")
  c_ref <- mdl2$get_data(names = "c")
  mdl2$solve(options = list(report = "none"))
  expect_equal(mdl2$get_data(names = "c"), c_ref)
  expect_equal(mdl2$get_ca(), mdl$get_ca())

  # now make all equations active again, and solve
  mdl2$set_eq_status("active")
  mdl2$solve(options = list(report = "none"))
  expect_equal(mdl2$get_data(period = "2015Q2/"),
               mdl$get_data(period = "2015Q2/"))
})

test_that("deactivating fixed equations", {
  mdl2 <- mdl$copy()
  c_ref <- mdl2$get_data(names = "c")
  mdl2$set_fix_values(650, names = "c", period = "/2015Q3")
  c_ref["/2015Q3", ] <- 650
  expect_equal(mdl2$get_data(names = "c"), c_ref)
  mdl2$set_eq_status("inactive", names = "c")
  mdl2$solve(options = list(report = "none"))
  expect_equal(mdl2$get_data(names = "c"), c_ref)
  expect_equal(mdl2$get_ca(names = "c"), mdl$get_ca(names = "c"))
  mdl2$run_eqn(names = "c")
  expect_equal(mdl2$get_data(names = "c"), c_ref)
  expect_equal(mdl2$get_ca(names = "c"), mdl$get_ca(names = "c"))
})

test_that("fixing deactived equation give an error", {
  mdl2 <- mdl$copy()
  mdl2$set_eq_status("inactive", names = "c")
  mdl2$set_fix_values(650, names = "c", period = "/2015Q3")

})

