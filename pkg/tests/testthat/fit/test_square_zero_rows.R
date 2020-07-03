library(isismdl)
library(testthat)

rm(list = ls())

context("fit square mdl zero rows")

mdl <- isis_mdl("mdl/square.mdl", period = 1000, silent = TRUE)

fit <- regts(matrix(c(0.412, 0.202, 0.813, 0.642, 0.123, 0.245), nrow = 1),
             period = 1000, names = paste0("w", 1:6))

rms <- rep(1, 6)
names(rms) <- paste0("x", 1:6)

mdl$set_fit(fit)
mdl$set_rms(rms)

test_that("row with almost zeros", {
  mdl2 <- mdl$copy()
  mdl2$set_param(list(r2 = c(0, 0, 1e-9, 0, 0, 0)))
  mdl2$set_fit_options(scale_method = "none", warn_ca = FALSE)
  expect_warning(output <- capture_output(mdl2$solve()),
                 "Simulation not possible")
  expect_known_output(cat(output),
                      "expected_output/square_zero_rows_rep1.txt")
  mdl2$set_fit_options(scale_method = "row")
  expect_silent(mdl2$solve(options = list(report = "none")))
  expect_identical(mdl2$get_solve_status(), "OK")
  expect_equal(mdl2$get_data(pattern = "^w\\d"), fit, tolerance = 1e-6)
})

test_that("row with only zeros", {
  mdl2 <- mdl$copy()
  mdl2$set_fit_options(scale_method = "row")
  mdl2$set_param(list(r2 = c(0, 0, 0, 0, 0, 0)))
  expect_warning(output <- capture_output(mdl2$solve()),
                 "Simulation not possible")
  expect_known_output(cat(output),
                      "expected_output/square_zero_rows_rep2.txt")
})
