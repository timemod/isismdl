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

old_data <- mdl$get_data()

test_that("set_fix update mode upd", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_fix(fix)

  new_data <- ts_update(old_data, fix_ordered, method = "tsupdval")
  # ts_update does not handle labels correctly.
  # this should be fixed in the future
  new_data <- update_ts_labels(new_data, ts_labels(old_data))

  # get_fix currently does not return labels
  expect_equal(mdl2$get_fix(), fix_ordered)
  expect_equal(mdl2$get_data(), new_data)

  mdl3 <- mdl$clone(deep = TRUE)
  mdl3$set_fix(fix, upd_mode = "upd")
  expect_equal(mdl3$get_fix(), fix_ordered)
  expect_equal(mdl3$get_data(), new_data)
})

test_that("set_fix for update mode upd, second test", {
  mdl2 <- mdl$clone(deep = TRUE)

  fix2 <- fix
  fix2["2015q2", "i"] <- NA
  fix2["2015q3", "c"] <- 650

  mdl2$set_fix(fix)
  mdl2$set_fix(fix2, upd_mode = "upd")

  fix_combi <- ts_update(fix, fix2, method = "tsupd")[, "c", drop = FALSE]
  # update labels (ts_update does not handle labels correctly yet)
  fix_combi <- update_ts_labels(fix_combi, ts_labels(fix))

  upd1<- ts_update(fix, fix2, method = "tsupdval")[, c("c", "i")]
  # update labels (ts_update does not handle labels correctly yet)
  upd1 <- update_ts_labels(upd1, ts_labels(fix))

  new_data <- ts_update(old_data, upd1, method = "tsupdval")
  # ts_update does not handle labels correctly.
  # this should be fixed in the future
  new_data <- update_ts_labels(new_data, ts_labels(old_data))

  expect_equal(mdl2$get_fix(), fix_combi)
  expect_equal(mdl2$get_data(), new_data)
})


test_that("set_fix for update mode updval", {
   mdl2 <- mdl$clone(deep = TRUE)

   fix2 <- fix
   fix2["2015q2", "i"] <- NA
   fix2["2015q3", "c"] <- 650

   mdl2$set_fix(fix)
   mdl2$set_fix(fix2, upd_mode = "updval")

   fix_combi <- ts_update(fix, fix2, method = "tsupdval")[, c("c", "i")]
   # update labels (ts_update does not handle labels correctly yet)
   fix_combi <- update_ts_labels(fix_combi, ts_labels(fix))

   new_data <- ts_update(old_data, fix_combi, method = "tsupdval")
   # ts_update does not handle labels correctly.
   # this should be fixed in the future
   new_data <- update_ts_labels(new_data, ts_labels(old_data))

   expect_equal(mdl2$get_fix(), fix_combi)
   expect_equal(mdl2$get_data(), new_data)
})



