library(utils)
library(isismdl)
library(testthat)

context("set_fix (1) for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

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
fix_mdl$set_fix(fix)

test_that("set_fix update mode upd", {

  new_data <- update_ts(old_data, fix_ordered, method = "updval")
  # update_ts does not handle labels correctly.
  # this should be fixed in the future
  new_data <- update_ts_labels(new_data, ts_labels(old_data))

  # get_fix currently does not return labels
  expect_equal(fix_mdl$get_fix(), fix_ordered)
  expect_equal(fix_mdl$get_data(), new_data)

  fix_mdl2 <- fix_mdl$clone(deep = TRUE)
  fix_mdl2$set_fix(fix, upd_mode = "upd")
  expect_equal(fix_mdl2$get_fix(), fix_ordered)
  expect_equal(fix_mdl2$get_data(), new_data)
})

test_that("set_fix for update mode upd, second test", {
  fix_mdl2 <- fix_mdl$copy()
  fix_mdl2$set_fix(fix2, upd_mode = "upd")

  fix_combi <- update_ts(fix, fix2, method = "upd")[, "c", drop = FALSE]
  # update labels (update_ts does not handle labels correctly yet)
  fix_combi <- update_ts_labels(fix_combi, ts_labels(fix))

  upd1<- update_ts(fix, fix2, method = "updval")[, c("c", "i")]
  # update labels (update_ts does not handle labels correctly yet)
  upd1 <- update_ts_labels(upd1, ts_labels(fix))

  new_data <- update_ts(old_data, upd1, method = "updval")
  # update_ts does not handle labels correctly.
  # this should be fixed in the future
  new_data <- update_ts_labels(new_data, ts_labels(old_data))

  expect_equal(fix_mdl2$get_fix(), fix_combi)
  expect_equal(fix_mdl2$get_data(), new_data)
})


test_that("set_fix for update mode updval", {
  fix_mdl2 <- fix_mdl$clone(deep = TRUE)
  fix_mdl2$set_fix(fix2, upd_mode = "updval")

  fix_combi <- update_ts(fix, fix2, method = "updval")[, c("c", "i")]
  # update labels (update_ts does not handle labels correctly yet)
  fix_combi <- update_ts_labels(fix_combi, ts_labels(fix))

  new_data <- update_ts(old_data, fix_combi, method = "updval")
  # update_ts does not handle labels correctly.
  # this should be fixed in the future
  new_data <- update_ts_labels(new_data, ts_labels(old_data))

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
