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

# test_that("set_ca update mode updval", {
#   mdl2 <- mdl$clone(deep = TRUE)
#   mdl2$set_ca(new_ca, upd_mode = "updval")
#   expect_equal(mdl2$get_ca(), mdl$get_ca())
# })



