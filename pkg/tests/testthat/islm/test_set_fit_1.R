library(utils)
library(isismdl)
library(testthat)

context("set_fit (1) for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

i <- regts(200, start = '2015Q2')
c <- regts(c(600, NA, 600), start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fit <- cbind(i, c, y)
ts_labels(fit) <- c("investment", "consumption", "income")

# an ordered list of fit values without identities:
fit_ordered <- fit[, c("c", "i", "y")]

test_that("set_fit update mode upd", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_fit(fit)

  # get_fit currently does not return labels
  expect_equal(mdl2$get_fit(), fit_ordered)

  # the model data should not have changed
  expect_equal(mdl2$get_data(), mdl$get_data())

  # mdl3 <- mdl$clone(deep = TRUE)
  # mdl3$set_ca(new_ca, upd_mode = "upd")
  # expect_equal(mdl2$get_ca(), new_ca)
})

test_that("set_fit for update mode upd (second test)", {
  mdl2 <- mdl$clone(deep = TRUE)

  fit2 <- fit
  fit2["2015q2", "i"] <- NA
  fit2["2015q3", "c"] <- 650

  mdl2$set_fit(fit)
  mdl2$set_fit(fit2, upd_mode = "upd")

  fit_combi <- ts_update(fit, fit2, method = "tsupd")[, c("c", "y")]
  # update labels (ts_update does not handle labels correctly yet)
  fit_combi <- update_ts_labels(fit_combi, ts_labels(fit))

  expect_equal(mdl2$get_fit(), fit_combi)
  expect_equal(mdl2$get_data(), mdl$get_data())
})


test_that("set_fit for update mode updval", {
  mdl2 <- mdl$clone(deep = TRUE)

  fit2 <- fit
  fit2["2015q2", "i"] <- NA
  fit2["2015q3", "c"] <- 650

  mdl2$set_fit(fit)
  mdl2$set_fit(fit2, upd_mode = "updval")

  fit_combi <- ts_update(fit, fit2, method = "tsupdval")[, c("c", "i", "y")]
  # update labels (ts_update does not handle labels correctly yet)
  fit_combi <- update_ts_labels(fit_combi, ts_labels(fit))

  expect_equal(mdl2$get_fit(), fit_combi)
  expect_equal(mdl2$get_data(), mdl$get_data())
})



