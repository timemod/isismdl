library(utils)
library(isismdl)
library(testthat)

context("run_eqn for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))

test_that("run_eqn for a small sample period", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_values(names = "c", value = NA, period = "2015Q3/2016Q1")
  mdl2$set_values(names = "t", value = NA, period = "2015Q4")
  mdl2$run_eqn(period = "2015Q3/2016Q1")
  expect_equal(mdl2$get_data(), mdl$get_data())
})

test_that("run_eqn for the full data period", {
  mdl2 <- mdl$clone(deep = TRUE)

  # fix t in the lag period, otherwise y and yd will be updated
  t <- mdl$get_data(period = "2015Q1", names = "y") -
       mdl$get_data(period = "2015Q1", names =  "yd")
  mdl2$set_fix(t, names = "t")

  mdl2$set_values(names = "c", value = NA, period = "2015Q3/2016Q1")
  mdl2$set_values(names = "t", value = NA, period = "2015Q4")
  mdl2$run_eqn()
  p <- mdl2$get_period()
  expect_equal(mdl2$get_data(period = p), mdl$get_data(period = p))

  md_correct <- regts(matrix(199.98375, ncol = 1), period = "2015Q1",
                      names = "md", labels = "money demand")
  md <- mdl2$get_data(names = "md", period = "2015Q1")
  expect_equal(md, md_correct)
})
