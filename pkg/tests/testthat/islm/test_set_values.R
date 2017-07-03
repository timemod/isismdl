library(utils)
library(isismdl)
library(testthat)

context("set_values for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

new_data <- mdl$get_data()
new_data[ , "c" ] <- 600
new_data["2015Q3/2016Q2", c("ms", "md")] <- c(200, 210, 215, 220)
new_data["2016Q2", c("y", "yd")] <- 990

test_that("set_values works correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_values(600, names = "c")
  mdl2$set_values(c(200, 210, 215, 220), names = c("ms", "md"),
                  period = "2015Q3/2016Q2")
  mdl2$set_values(990, pattern = "^y", period = "2016Q2")
  expect_equal(mdl2$get_data(), new_data)
})

test_that("set_values handles errors correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  msg <- "xxx is not a model variable"
  expect_error(mdl2$set_values(1, names = c("y", "xxx")), msg)
  msg <- "The variables p xxx are no model variables"
  expect_error(mdl2$set_values(1, names = c("p", "xxx")), msg)
  msg <- paste("Period 2012 has a different frequency than the model",
               "period 2015Q2/2016Q3.")
  expect_error(mdl2$set_values(600, names = "c", period = "2012Y"), msg)
})
