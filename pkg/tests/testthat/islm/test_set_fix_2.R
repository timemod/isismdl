library(testthat)
library(isismdl)


# prepare rms values and fix targets
i <- regts(200, start = '2015Q2')
c <- regts(c(990, NA, 1010), start = '2015Q2')
fix <- cbind(c, i)
ts_labels(fix) <- c("consumption", "investment")

capture_output(mdl <- read_mdl("islm_model.ismdl"))

mdl$set_fix(fix)

test_that("Testing get_fix after cloning", {
  expect_identical(mdl$get_fix(), fix)
  mdl2 <- mdl$clone(deep = TRUE)
  expect_identical(mdl2$get_fix(), fix)
  mdl2$clear_fix()
  expect_null(mdl2$get_fix())
  expect_identical(mdl$get_fix(), fix)
  mdl3 <- mdl$clone(deep = TRUE)
  md <- regts(200, start = '2015Q2')
  t <- regts(c(990, NA, 1010), start = '2015Q2')
  fix2 <- cbind(md, t)
  ts_labels(fix2) <- c("money demand", "tax")
  mdl3$set_fix(fix2)

  expect_identical(mdl$get_fix(), fix)
  res_correct <- cbind(fix, fix2)
  res_correct <- res_correct[, order(colnames(res_correct))]
  expect_identical(mdl3$get_fix(), res_correct)
})

test_that("Testing fix values after reading the model", {
   mdl$write_mdl("temp.ismdl")
   capture.output(mdl4 <- read_mdl("temp.ismdl"))
   unlink("temp.ismdl")
   expect_identical(mdl4$get_fix(), fix)
})

