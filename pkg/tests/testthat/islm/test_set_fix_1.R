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

test_that("set_fix update mode upd", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_fix(fix)
  old_data <- mdl$get_data()

  new_data <- ts_update(old_data, fix, method = "tsupdval")
  # ts_update does not handle labels correctly.
  # this should be fixed in the future
  new_data <- update_ts_labels(new_data, ts_labels(old_data))

  # get_fix currently does not return labels
  expect_equal(mdl2$get_fix(), fix_ordered)

  expect_equal(mdl2$get_data(), new_data)

  # mdl3 <- mdl$clone(deep = TRUE)
  # mdl3$set_ca(new_ca, upd_mode = "upd")
  # expect_equal(mdl2$get_ca(), new_ca)
})

# test_that("set_ca update mode updval", {
#   mdl2 <- mdl$clone(deep = TRUE)
#   mdl2$set_ca(new_ca, upd_mode = "updval")
#   expect_equal(mdl2$get_ca(), mdl$get_ca())
# })



