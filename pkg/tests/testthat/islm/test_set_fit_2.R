library(testthat)
library(isismdl)

rm(list = ls())


# prepare rms values and fit targets
i <- regts(200, start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fit_targets <- cbind(y, i)
ts_labels(fit_targets) <- c("income", "investment")

fit_targets_sorted <- fit_targets[, c("i", "y")]

capture_output(mdl <- read_mdl("islm_model.ismdl"))

mdl$set_fit(fit_targets)

test_that("Testing get_fit after cloning", {
  expect_identical(mdl$get_fit(), fit_targets_sorted)
  mdl2 <- mdl$clone(deep = TRUE)
  expect_identical(mdl2$get_fit(), fit_targets_sorted)
  mdl2$clear_fit()
  expect_null(mdl2$get_fit())
  expect_identical(mdl$get_fit(), fit_targets_sorted)
  mdl3 <- mdl$clone(deep = TRUE)
  c <- regts(200, start = '2015Q2')
  t <- regts(c(990, NA, 1010), start = '2015Q2')
  fit_targets2 <- cbind(c, t)
  ts_labels(fit_targets2) <- c("consumption", "tax")
  mdl3$set_fit(fit_targets2)
  expect_identical(mdl$get_fit(), fit_targets_sorted)
  res_correct <- cbind(fit_targets_sorted, fit_targets2)
  res_correct <- res_correct[, order(colnames(res_correct))]
  expect_identical(mdl3$get_fit(), res_correct)
})

test_that("Testing fit_targets after reading the model", {
  mdl$write_mdl("temp.ismdl")
  capture.output(mdl4 <- read_mdl("temp.ismdl"))
  unlink("temp.ismdl")
  expect_identical(mdl4$get_fit(), fit_targets_sorted)
})

