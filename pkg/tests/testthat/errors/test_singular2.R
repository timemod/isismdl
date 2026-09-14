library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/solve_report_tools.R")

mdl <- isis_mdl("mdl/singular2.mdl", period = "2011", silent = TRUE)
mdl$set_values(0)
mdl$set_solve_options(maxiter = 1)

test_that("solve", {

  expect_warning(
    report <- capture.output(mdl$solve(options = list(store_jac = TRUE))),
    "Simulation stopped"
  )

  expect_known_report(report,
                      "expected_output/singular2_rep1.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)

  # try svd analyis, but decrease sv_tol because this jacobian is only
  # slightly singular.

  jac <- mdl$get_solve_jac()
  result <- svd_analysis(jac)
  expect_equal(result$rank, 5)
  expect_output(print(result), "Full numerical rank")

  result <- svd_analysis(jac, sv_tol = 1e-5)
  expect_output(print(result), "singular vectors")
  expect_equal(result$rank, 4)
  expect_true(length(result$small_sv) == 1)
  expect_equal(dim(result$U_small), c(2, 1))
  expect_equal(dim(result$V_small), c(2, 1))
  expect_equal(result$lin_dep_rows, 3:4)
  expect_equal(result$lin_dep_cols, 3:4)
  expect_equal(length(result$zero_rows), 0)
  expect_equal(length(result$zero_cols), 0)
})
