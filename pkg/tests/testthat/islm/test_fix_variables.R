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

  mdl2$fix_variables(pattern = ".*")
  expect_equal(mdl2$get_fix(),
               mdl$get_data(names = mdl$get_endo_names(type = "frml"),
                            period = mdl$get_period()))
})


test_that("errors", {
  mdl2 <- mdl$copy()
  mdl2$set_values(NA, names = "c", period = "2015Q3")
  msg <- paste0("Can't fix variable\\(s\\) c.\n",
       "The model variables contain\\(s\\) NA values in period 2015Q3/2015Q4")
  expect_error(mdl2$fix_variables(names = "c", period = "2015Q3/2015Q4"),
               msg)

  capture.output(mdl2 <- islm_mdl())
  msg <- paste("The model period is not set.",
               "Set the model period with set_period\\(\\) or init_data\\(\\).")
  expect_error(mdl2$fix_variables(names = "c"), msg)
})