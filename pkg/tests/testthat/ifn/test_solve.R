context("solve IFN model")

library(utils)

capture_output(ifn_mdl <- read_mdl("ifn_mdl.rds"))

test_that("ftrelax correctly read from file", {
  res_correct <- c(a = NA, eta = NA, lambda = 0.5, mzk = NA, px = NA, rho = NA)
  expect_identical(ifn_mdl$get_ftrelax(), res_correct)
})

test_that("solve options read from file", {
  options_set <- list(xmaxiter = 1500, ratreport = "iter",
                      report = "minimal", ratreport_rep = 10,
                      ratfullreport_rep = 50)
  expect_equal(ifn_mdl$get_solve_options()[names(options_set)], options_set)
})

report <- ifn_mdl$solve(options = list(report = "none"))

isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)

mdl_per <- ifn_mdl$get_period()
dif <- tsdif(ifn_mdl$get_data(period = mdl_per), isis_result, tol = 1e-6,
             fun = cvgdif)

test_that("solve result almost identical to Isis result", {
  expect_identical(ifn_mdl$get_data_period(), period_range("1y", "101y"))
  expect_identical(ifn_mdl$get_var_names(), colnames(isis_result))
  expect_identical(dif$missing_names1, character(0))
  expect_identical(dif$missing_names2, character(0))
  expect_identical(dif$difnames, character(0))
})

