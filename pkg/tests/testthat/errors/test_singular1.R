library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

source("../tools/solve_report_tools.R")

mdl <- isis_mdl("mdl/singular1.mdl", period = "2011", silent = TRUE)
mdl$set_values(0)
mdl$set_solve_options(dbgopt = c("prifb", "prijac"), store_jac = TRUE,
                      maxiter = 1)

test_that("solve", {

  expect_warning(
    report <- capture.output(mdl$solve()),
    "Simulation stopped"
  )
  expect_known_report(report,
                      "expected_output/singular1_rep1.txt",
                      update = update_expected,
                      replace_all_numbers = TRUE)

  # Test SVD analysis
  jac <- mdl$get_solve_jac()
  result <- svd_analysis(jac)
  expect_equal(result$rank, 4)
  expect_true(length(result$small_sv) == 1)
  expect_equal(dim(result$U_small), c(2, 1))
  expect_equal(dim(result$V_small), c(3, 1))
  expect_equal(result$lin_dep_rows, 1:2)
  expect_equal(result$lin_dep_cols, c(1, 2, 5))
  expect_equal(length(result$zero_rows), 0)
  expect_equal(length(result$zero_cols), 0)

  # No output when solve options report ="none":
  mdl$set_values(0)
  expect_silent(
    expect_warning(
      mdl$solve(options = list(report = "none")),
      "Simulation stopped"
    )
  )
  expect_identical(mdl$get_solve_status(), "Simulation stopped")
})
