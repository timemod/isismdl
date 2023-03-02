library(testthat)
library(isismdl)

rm(list = ls())


mdl_basis <- read_mdl("islm_model_solved.ismdl", silent = TRUE)
msp <- mdl_basis$get_period()
nmsp <- nperiod(msp)
mdp <- mdl_basis$get_data_period()

mdl_new <- mdl_basis$copy()

test_that("deactive y", {

  mdl_new$set_eq_status(names = "y", status = "inactive")

  # at this stage, y is inactive but still in solve order
  expect_equal(mdl_new$get_eq_names(order = "solve"),
               c("i", "md", "t", "yd", "c", "y", "r"))
  expect_equal(mdl_new$get_eq_names(order = "solve", status = "active"),
               c("i", "md", "t", "yd", "c", "r"))
  expect_equal(mdl_new$get_eq_names(order = "solve", status = "inactive"),
               c("y"))
  expect_equal(mdl_new$get_eq_names(order = "natural", status = "inactive"),
               c("y"))

  # check that y is not modifief when solve is called
  mdl_new$set_values(names = "y", value = 999, period = msp)
  mdl_new$solve(options = list(report = "none"))
  expect_equal(mdl_new$get_data(names = "y")[, 1],
               regts(c(980, rep(999, nmsp)), period = mdp, labels = "income"))

  # check get_dep_struct
  dep_struct <- mdl_new$get_dep_struct()
  expect_known_value(dep_struct,
                     "expected_output/inactive_order_dep_struct.rds")

  # now reorder the model, y should no longer be in solve order
  mdl_new$order(silent = TRUE)
  expect_equal(mdl_new$get_eq_names(order = "solve"),
               c("i", "t", "yd", "md", "r", "c"))
  expect_equal(mdl_new$get_eq_names(order = "solve", status = "active"),
               c("i", "t", "yd", "md", "r", "c"))
  expect_equal(mdl_new$get_eq_names(order = "solve", status = "inactive"),
               character(0))


  dep_struct_2 <- mdl_new$get_dep_struct()
  expect_known_value(dep_struct_2,
                     "expected_output/inactive_order_dep_struct.rds",
                     update = FALSE)


  # check solve again
  mdl_new$set_values(names = "y", value = 1000, period = msp)
  mdl_new$solve(options = list(report = "none"))
  expect_equal(mdl_new$get_data(names = "y")[, 1],
               regts(c(980, rep(1000, nmsp)) , period = mdp, labels = "income"))


})

test_that("reactivate y", {
  # now reactivate "y"
  mdl_new$set_eq_status(names = "y", status = "active")

  emsg <- "One or more active equations not in solve order. Reorder the model with method 'order'."

  # check get_eq_status
  expect_error(mdl_new$get_eq_names(order = "solve"), emsg, fixed = TRUE)
  expect_error(mdl_new$get_eq_names(pattern = "^y", order = "solve",
                                    status = "active"),
               emsg, fixed = TRUE)
  expect_equal(mdl_new$get_eq_names(order = "solve", status = "inactive"),
               character(0))
  expect_equal(mdl_new$get_eq_names(status = "active"),
               mdl_basis$get_eq_names())

  # solve the model we should now get an error
  expect_error(mdl_new$solve(options = list(report = "none")), emsg, fixed = TRUE)
  expect_error(mdl_new$fill_mdl_data(), emsg, fixed = TRUE)
  expect_error(mdl_new$run_eqn(), emsg, fixed = TRUE)
  expect_error(mdl_new$run_eqn(names = "y", solve_order = TRUE), emsg,
               fixed = TRUE)

  # we should still be able to run "y", even though this variable is not
  # in solve_oder
  expect_silent(mdl_new$run_eqn(names = "y", period = msp))
  data <- mdl_new$get_data()
  y_expected <- data$y
  y_expected[msp] <- (data$c + data$i + data$g)[msp]
  expect_equal(y_expected, data$y)

  # now reorder again and solve
  mdl_new$order(silent = TRUE)
  mdl_new$solve(options = list(report = "none"))
  expect_equal(mdl_basis$get_data(), mdl_new$get_data())
})
