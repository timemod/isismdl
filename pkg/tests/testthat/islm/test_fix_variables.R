library(utils)
library(isismdl)
library(testthat)

context("fix_variables for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))


test_that("fix_values works correctly", {
  mdl2 <- mdl$copy()
  mdl2$fix_variables(names = "c", period = "2015Q3/2015Q4")
  mdl2$fix_variables(pattern = "^t$")
  c_correct <- mdl$get_data(names = "c", period = "2015Q3/2015Q4")
  t_correct <- mdl$get_data(names = "t", period = mdl$get_period())
  expect_equal(mdl2$get_fix(), cbind(c_correct, t_correct))
})
