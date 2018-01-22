library(utils)
library(isismdl)
library(testthat)

context("set_data argument fun for the  ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))

old_data <- mdl$get_data()
old_ca <- mdl$get_ca()

test_that("univariate timeseries", {
  mdl2 <- mdl$copy()
  mdl2$set_data(regts(-5, period = "2015Q2/2015Q4"), names = "c", fun = `+`)
  expected_data <- old_data
  expected_data["2015Q2/2015Q4", "c"] <- expected_data["2015Q2/2015Q4", "c"] - 5
  expect_equal(mdl2$get_data(), expected_data)
})

test_that("multivariate timeseries", {

  # prepare shock
  g <- regts(c(1.1, 1.2), period = "2015Q2/2015Q3")
  ms <-  regts(1.2, period = "2015Q2")
  x <- 2 * ms
  shock <- cbind(g, ms, x)

  mdl2 <- mdl$copy()
  mdl2$set_data(shock, fun = `/`)
  expected_data <- old_data
  expected_data["2015Q2/2015Q3", c("g", "ms")] <-
    expected_data["2015Q2/2015Q3", c("g", "ms")] / shock[, c("g", "ms")]
  expect_equal(mdl2$get_data(), expected_data)

  mdl3 <- mdl$copy()
  mdl3$set_data(shock, fun = `/`, upd_mode = "updval")
  expected_data <- old_data
  expected_data["2015Q2/2015Q3", "g"] <- expected_data["2015Q2/2015Q3", "g"] / g
  expected_data["2015Q2", "ms"] <- expected_data["2015Q2", "ms"] / ms
  expect_equal(mdl3$get_data(), expected_data)
})

test_that("name error", {
  G <- regts(c(1.1, 1,2), period = "2015Q2/2015Q3")
  MS <-  regts(1.2, period = "2015Q2")
  shock <- cbind(G, MS)
  mdl2 <- mdl$copy()
  mdl2$set_data(shock)
  expect_equal(mdl$get_data(), mdl2$get_data())
})

test_that("constant adjustment", {

  # prepare shock
  c <- regts(c(5, 10), period = "2015Q2/2015Q3")
  i <-  regts(5, period = "2015Q2")
  shock <- cbind(c, i)

  mdl2 <- mdl$copy()
  mdl2$set_ca(shock, fun = `-`)
  expected_ca <- old_ca
  expected_ca["2015Q2/2015Q3", c("c", "i")] <-
    expected_ca["2015Q2/2015Q3", c("c", "i")] - shock[, c("c", "i")]
  expect_equal(mdl2$get_ca(), expected_ca)
})



