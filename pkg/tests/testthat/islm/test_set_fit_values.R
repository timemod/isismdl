library(utils)
library(isismdl)
library(testthat)

context("set_fix_values for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

i <- regts(200, start = '2015Q2')
c <- regts(c(600, NA, 600), start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fit <- cbind(c, i, y)
ts_labels(fit) <- c("consumption", "investment", "income")
mdl$set_fit(fit)

new_fit <- fit[mdl$get_data_period(), ]
new_fit[ , "t" ] <- 210
new_fit["2015Q3/2016Q2", c("y", "c")] <- 810:813
new_fit["2016Q2", "i"] <- 190
new_fit <- new_fit[, order(colnames(new_fit))]
new_fit <- update_ts_labels(new_fit, c(t = "tax"))

test_that("set_fix_values works correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_fit_values(210, names = "t")
  mdl2$set_fit_values(810:813, names = c("y", "c"), period = "2015Q3/2016Q2")
  mdl2$set_fit_values(190, pattern = "^i$", period = "2016Q2")
  expect_equal(mdl2$get_fit(), new_fit)
  mdl2$set_fit_values(NA, names = "yd")
  expect_equal(mdl2$get_fit(), new_fit)
  mdl2$set_fit_values(NA, names = "t")
  expect_equal(mdl2$get_fit(), new_fit["2015Q2/2016Q2" , c("c", "i", "y")])
  mdl2$set_fit_values(NA, period = "2016Q1/")
  expect_equal(mdl2$get_fit(), new_fit["2015Q2/2015Q4" , c("c", "i", "y")])
})

test_that("set_fit_values handles errors correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  msg <- "xxx is not a model variable"
  expect_error(mdl2$set_fit_values(1, names = c("y", "xxx")), msg)
  msg <- "The variables p xxx are no model variables"
  expect_error(mdl2$set_fit_values(1, names = c("p", "xxx")), msg)
})
