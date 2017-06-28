library(utils)
library(isismdl)
library(testthat)

context("get_ca for the  ISLM model")

capture_output(mdl <- read_mdl("islm_model.rds"))

names <- mdl$get_var_names(type = "allfrml")
nperiod <- nperiod(mdl$get_data_period())
set.seed(123)
ca <- matrix(rnorm(nperiod * length(names)), nrow = nperiod)
labels <- paste(mdl$get_labels()[names], "(constant adjustment)")
ca <- regts(ca, period = mdl$get_data_period(), names = names, labels = labels)
mdl$set_ca(ca)

test_that("get_ca works correctly", {
  expect_equal(mdl$get_ca(), ca)
  expect_equal(mdl$get_ca(period = "2016Q1/"), ca["2016Q1/2016Q3", ])
  expect_equal(mdl$get_ca(period = "/2016Q1", names = c("c", "i")),
               ca["2015Q1/2016Q1", c("c", "i")])
  expect_equal(mdl$get_ca(period = "2016Q1/2016Q3", pattern = "^t"),
               ca["2016Q1/2016Q3", "t", drop = FALSE])
})
