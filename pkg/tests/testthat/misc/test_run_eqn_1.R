library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE


mdl_file <- "mdl/test_run_eqn_1.mdl"

lag_data <- regts(matrix(1:2, ncol = 2), start = 2020,
                   names = c("x1", "y1"))
lead_data <- regts(matrix(3:4, ncol = 2), start = 2025,
                   names = c("x2", "y2"))
init_data <- cbind(lag_data, lead_data)

solve_per <- period_range(2021, 2023)
mdl <- isis_mdl(mdl_file, silent = TRUE, period = solve_per,
                data = init_data)

test_that("run_eqn with correct solve order and forwards", {

  mdl$run_eqn(names = c("x1", "y1"), period = period_range(2021, 2025),
              solve_order = TRUE)
  mdl$run_eqn(names = c("x2", "y2"), period = period_range(2020, 2024),
              forwards = FALSE, solve_order = TRUE)

  data <- mdl$get_data()
  expect_known_value(data, "expected_output/run_eqn_1_data.rds",
                     update = update)
  expect_output(mdl$solve(),
                "Total number of iterations\\s+0")
})

test_that("argument update_mode", {
  mdl$set_values(NA, names = "y2", period = 2025)
  data_old <- mdl$get_data()
  mdl$run_eqn(names = "y2", forwards = FALSE, update_mode = "updval")
  expect_equal(mdl$get_data(), data_old)
  mdl$run_eqn(names = "y2", forwards = FALSE, update_mode = "upd")
  expected_result <- data_old
  expected_result$y2 <- NA
  expect_equal(mdl$get_data(), expected_result)
})

test_that("run_eqn without solve order", {
  mdl2 <- isis_mdl(mdl_file, silent = TRUE, period = solve_per,
                  data = init_data)

  mdl2$run_eqn(names = c("x1", "y1"), period = period_range(2021, 2025))
  mdl2$run_eqn(names = c("x2", "y2"), period = period_range(2020, 2024),
              forwards = FALSE)
  data <- mdl2$get_data()
  expected_result <- init_data[, colnames(data)]
  expected_result$y1 <- 2
  expected_result$y2 <- 4
  expect_equal(data, expected_result)

  # errors
  expect_error(mdl2$run_eqn(names = "jan"), "\"jan\" is not an active equation.",
               fixed = TRUE)

  msg <- "Argument 'forwards' should be a TRUE or FALSE"
  expect_error(mdl2$run_eqn(forwards = "yes"), msg, fixed = TRUE)
  expect_error(mdl2$run_eqn(forwards = NA), msg, fixed = TRUE)
  expect_error(mdl2$run_eqn(forwards = c(TRUE, TRUE)), msg, fixed = TRUE)
})



