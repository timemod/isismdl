library(utils)
library(isismdl)
library(testthat)

context("set_values for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))

test_that("set_values works correctly (1)" , {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_values(600, names = "c")
  mdl2$set_values(c(200, 210, 215, 220), names = c("ms", "md"),
                  period = "2015Q3/2016Q2")
  mdl2$set_values(990, pattern = "^y", period = "2016Q2")

  expected <- mdl$get_data()
  expected[ , "c" ] <- 600
  expected["2015Q3/2016Q2", c("ms", "md")] <- c(200, 210, 215, 220)
  expected["2016Q2", c("y", "yd")] <- 990

  expect_equal(mdl2$get_data(), expected)
})

test_that("set_values works correctly (2)", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_values(600, period = "2016", names = "c")
  mdl2$set_values(c(200, 210, 215, 220), names = c("ms", "md"),
                  period = "2015M6/2016M2")
  mdl2$set_values(990, pattern = "^y", period = "2016M4/")

  expected <- mdl$get_data()
  expected["2016q1/2016q3" , "c" ] <- 600
  expected["2015Q2/2016Q1", c("ms", "md")] <- c(200, 210, 215, 220)
  expected["2016Q2/2016Q3", c("y", "yd")] <- 990

  expect_equal(mdl2$get_data(), expected)
})

test_that("set_values handles errors correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  msg <- "xxx is not a model variable"
  expect_error(mdl2$set_values(1, names = c("y", "xxx")), msg)
  msg <- "The variables p xxx are no model variables"
  expect_error(mdl2$set_values(1, names = c("p", "xxx")), msg)
})
