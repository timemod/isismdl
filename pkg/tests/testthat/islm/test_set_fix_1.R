library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

context("set_fix (1) for the ISLM model")

mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

i <- regts(200, start = '2015Q2')
c <- regts(c(600, NA, 600), start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fix <- cbind(i, c, y)
ts_labels(fix) <- c("investment", "consumption", "income")

# an ordered list of fix values without identities:
fix_ordered <- fix[, c("c", "i")]

fix2 <- fix
fix2["2015q2", "i"] <- NA
fix2["2015q3", "c"] <- 650

old_data <- mdl$get_data()

fix_mdl <- mdl$copy()

test_that("set_fix update mode upd", {

  expect_warning(fix_mdl$set_fix(fix, name_err = "warn"),
                 "\"y\" is not a frml variable\\.")

  new_data <- update_ts(old_data, fix_ordered, method = "updval")

  # get_fix currently does not return labels
  expect_equal(fix_mdl$get_fix(), fix_ordered)
  expect_equal(fix_mdl$get_data(), new_data)

  fix_mdl2 <- fix_mdl$clone(deep = TRUE)
  fix_mdl2$set_fix(fix, upd_mode = "upd", name_err = "silent")
  expect_equal(fix_mdl2$get_fix(), fix_ordered)
  expect_equal(fix_mdl2$get_data(), new_data)
})

test_that("set_fix for update mode upd, second test", {
  fix_mdl2 <- fix_mdl$copy()
  expect_error(fix_mdl2$set_fix(fix2, upd_mode = "upd", name_err = "stop"),
               "\"y\" is not a frml variable.")
  fix_mdl2$set_fix(fix2, upd_mode = "upd", name_err = "silent")
  expect_silent(fix_mdl2$set_fix(fix2, upd_mode = "upd"))
  fix_combi <- update_ts(fix, fix2, method = "upd")[, "c", drop = FALSE]

  upd1 <- update_ts(fix, fix2, method = "updval")[, c("c", "i")]

  new_data <- update_ts(old_data, upd1, method = "updval")

  expect_equal(fix_mdl2$get_fix(), fix_combi)
  expect_equal(fix_mdl2$get_data(), new_data)
})


test_that("set_fix for update mode updval", {
  fix_mdl2 <- fix_mdl$copy()
  fix_mdl2$set_fix(fix2, upd_mode = "updval", name_err = "silent")

  fix_combi <- update_ts(fix, fix2, method = "updval")[, c("c", "i")]

  new_data <- update_ts(old_data, fix_combi, method = "updval")

  expect_equal(fix_mdl2$get_fix(), fix_combi)
  expect_equal(fix_mdl2$get_data(), new_data)
})

test_that("set_fix_values", {
  fix_mdl2 <- fix_mdl$copy()
  fix_mdl2$set_fix_values(NA, names = "c", period = "2015Q4")
  expect_equal(fix_mdl2$get_fix(), fix_ordered["2015Q2", , drop = FALSE])
  expect_equal(fix_mdl2$get_data(), fix_mdl$get_data())

  fix_mdl2$set_fix_values(NA, names = c("c", "i"), period = "2015Q2")
  expect_null(fix_mdl2$get_fit())
})
