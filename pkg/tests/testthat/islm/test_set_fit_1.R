library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

context("set_fit (1) for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))

i <- regts(200, start = '2015Q2')
c <- regts(c(600, NA, 600), start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fit <- cbind(i, c, y)
ts_labels(fit) <- c("investment", "consumption", "income")

# an ordered list of fit values without identities:
fit_ordered <- fit[, c("c", "i", "y")]

fit2 <- fit
fit2["2015q2", "i"] <- NA
fit2["2015q3", "c"] <- 650

fit_mdl <- mdl$copy()
fit_mdl$set_fit(fit)

test_that("set_fit update mode upd", {

  # get_fit currently does not return labels
  expect_equal(fit_mdl$get_fit(), fit_ordered)

  # the model data should not have changed
  expect_equal(fit_mdl$get_data(), mdl$get_data())

  fit_mdl2 <- mdl$clone(deep = TRUE)
  fit_mdl2$set_fit(fit, upd_mode = "upd")

  expect_equal(fit_mdl2$get_fit(), fit_ordered)

  # the model data should not have changed
  expect_equal(fit_mdl2$get_data(), mdl$get_data())
})

test_that("set_fit for update mode upd (second test)", {
  fit_mdl2 <- fit_mdl$clone(deep = TRUE)
  fit_mdl2$set_fit(fit2, upd_mode = "upd")

  fit_combi <- update_ts(fit, fit2, method = "upd")[ ,  c("c", "y")]

  expect_equal(fit_mdl2$get_fit(), fit_combi)
  expect_equal(fit_mdl2$get_data(), mdl$get_data())
})


test_that("set_fit for update mode updval", {
  fit_mdl2 <- fit_mdl$copy()
  fit_mdl2$set_fit(fit2, upd_mode = "updval")

  fit_combi <- update_ts(fit, fit2, method = "updval")[, c("c", "i", "y")]

  expect_equal(fit_mdl2$get_fit(), fit_combi)
  expect_equal(fit_mdl2$get_data(), mdl$get_data())
})

test_that("set_fit_values", {
  fit_mdl2 <- fit_mdl$copy()
  fit_mdl2$set_fit_values(NA, names = c("c", "y"), period = "2015Q4")
  expect_equal(fit_mdl2$get_fit(), fit_ordered["2015Q2", , drop = FALSE])

  fit_mdl2$set_fit_values(NA, names = c("c", "i", "y"), period = "2015Q2")
  expect_null(fit_mdl2$get_fit())
})



