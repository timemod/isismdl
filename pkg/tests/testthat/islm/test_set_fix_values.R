library(utils)
library(isismdl)
library(testthat)

rm(list = ls())


mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

i <- regts(200, start = '2015Q2')
c <- regts(c(600, NA, 600), start = '2015Q2')
t <- regts(c(190, 195), start = '2015Q3')
fix <- cbind(c, i, t)
ts_labels(fix) <- c("consumption", "investment", "tax")
mdl$set_fix(fix)

new_fix <- fix[mdl$get_data_period(), ]
new_fix[ , "c" ] <- 620
new_fix["2015Q3/2016Q2", c("t", "i")] <- 210:213
new_fix["2016Q2", "t"] <- 190

test_that("set_fix_values works correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_fix_values(620, names = "c")
  mdl2$set_fix_values(210:213, names = c("t", "i"), period = "2015Q3/2016Q2")
  mdl2$set_fix_values(190, pattern = "^t$", period = "2016Q2")
  expect_equal(mdl2$get_fix(), new_fix)
  mdl2$set_fix_values(NA, names = "md")
  expect_equal(mdl2$get_fix(), new_fix)
  mdl2$set_fix_values(NA, names = "t")
  expect_equal(mdl2$get_fix(), new_fix[, c("c", "i")])
  mdl2$set_fix_values(NA, period = "2016Q1/")


  msg <- paste("Specified period \\(3200Q1/3200Q4\\) is completely outside the",
                      "data period \\(2015Q1/2016Q3\\)\\.")
  expect_warning(mdl2$set_fix_values(888, period = 3200), msg)


  expect_equal(mdl2$get_fix(), new_fix["2015Q1/2015Q4" , c("c", "i")])
})

test_that("set_fix_values handles errors correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  msg <- "\"y\" is not a frml variable"
  expect_error(mdl2$set_fix_values(1, names = "y"), msg)
  msg <-  "The following names are no frml variables: \"y\", \"xxx\"."
  expect_error(mdl2$set_fix_values(1, names = c("y", "xxx", "c")), msg)
})
