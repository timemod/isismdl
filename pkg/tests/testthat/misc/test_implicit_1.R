library(isismdl)
library(testthat)

rm(list = ls())

mdl_file <- "mdl/implicit_example1.mdl"
mdl <- isis_mdl(model_file = mdl_file, period = "2020/2025", silent = TRUE)

test_that("solve", {

  # Basic example ----

  mdl2 <- mdl$copy()
  mdl2$set_values(c(1, 2, 3), names = "x", period = "2022/2024")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$solve(options = list(erropt = "silent", report = "none"))
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(2, 4, 5, NA))

  # Non-zero CA values ----

  mdl2$set_ca_values(1:4, names = "x", period = "2022/2025")
  mdl2$solve(options = list(erropt = "silent", report = "none"))
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               sqrt(c(3, 14, 22, NA)))

  # NA CA values ----
  mdl2$set_ca_values(NA, names = "x", period = "2023")
  mdl2$solve(period = "2022/2025", options = list(erropt = "silent", report = "none"))
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               sqrt(c(3, NA, 22, NA)))
})

test_that("solve and fix", {
  mdl2 <- mdl$copy()
  mdl2$set_values(c(1, 2, 3), names = "x", period = "2022/2024")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$set_fix_values(333, names = "x", period = "2023")
  mdl2$solve(options = list(erropt = "silent", report = "none"))
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(2, 333, 5, NA))
  expect_equal(as.numeric(mdl2$get_ca(period = "2022/2025", names = "x")),
               c(0, -(333**2 - 16), 0, 0))
})

test_that("run_eqn", {

  # Basic example ----

  mdl2 <- mdl$copy()
  mdl2$set_values(1, names = "x", period = "2022")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$run_eqn(names = "x", period = "2022/2025")
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(2, 4, 5, 6))

  # CA values ----

  mdl2$set_ca_values(1:4, names = "x", period = "2022/2025")
  mdl2$run_eqn(names = "x", period = "2022/2025")
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               sqrt(c(3, 14, 22, 32)))

  # NA CA values ----
  mdl2$set_ca_values(NA, names = "x", period = "2023")
  mdl2$run_eqn(names = "x", period = "2022/2025")
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               sqrt(c(3, NA, 22, 32)))

  # Missing initial value
  mdl2$set_values(NA, names = "x", period = "2022/2025")
  expect_output(
    mdl2$run_eqn(names = "x", period = "2022/2025"),
    "Missing/invalid initial value in implicit equation\nin period"
  )
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               rep(NA_real_, 4))

  # No solution in one period ----
  mdl2$set_ca_values(0, names = "x")
  mdl2$set_values(1:2, names = "x", period = "2022/2023")
  mdl2$set_values(-10, names = "a", period = "2022")
  expect_output(
    mdl2$run_eqn(names = "x", period = "2022/2025"),
    "No convergence in 50 iterations\nin period 2022"
  )
  expect_equal(as.numeric(mdl2$get_data(period = "2023/2025", names = "x")),
               c(4, 5, 6))
})

test_that("run_eqn and fix", {
  mdl2 <- mdl$copy()
  mdl2$set_values(c(1, 2, 3), names = "x", period = "2022/2024")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$set_fix_values(333, names = "x", period = "2023")
  mdl2$run_eqn(names = "x", period = "2022/2025")
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(2, 333, 5, 6))
  expect_equal(as.numeric(mdl2$get_ca(period = "2022/2025", names = "x")),
               c(0, -(333**2 - 16), 0, 0))
})

test_that("fill_mdl_data include_frmls", {

  # Basic example ----
  mdl2 <- mdl$copy()
  mdl2$set_values(1, names = "x", period = "2022")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  expect_output(
    mdl2$fill_mdl_data(include_frmls = TRUE, report = "minimal"),
    "Replaced a total of 5 missing/invalid values",
    fixed = TRUE
  )
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(1, 4, 5, 6))

  # Non-zero CA values ----

  mdl2 <- mdl$copy()
  mdl2$set_values(1, names = "x", period = "2022")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$set_ca_values(1:4, names = "x", period = "2022/2025")
  expect_output(
    mdl2$fill_mdl_data(include_frmls = TRUE, report = "minimal"),
    "Replaced a total of 5 missing/invalid values",
    fixed = TRUE
  )
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(1, sqrt(c(14, 22, 32))))

  # NA CA values ----

  mdl2 <- mdl$copy()
  mdl2$set_values(1, names = "x", period = "2022")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$set_ca_values(c(1, 2, NA, 4), names = "x", period = "2022/2025")
  expect_output(
    mdl2$fill_mdl_data(include_frmls = TRUE, report = "minimal"),
    "Replaced a total of 3 missing/invalid values",
    fixed = TRUE
  )
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(1, sqrt(14), NA, NA))

  # NA value for X and one CA value
  mdl2 <- mdl$copy()
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$set_ca_values(c(1, 2, NA, 4), names = "x", period = "2022/2025")
  expect_output(
    mdl2$fill_mdl_data(include_frmls = TRUE, report = "minimal"),
    "No missing/invalid values replaced",
    fixed = TRUE
  )
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               rep(NA_real_, 4))

  # Fix x (1) -----

  mdl2 <- mdl$copy()
  mdl2$set_values(1, names = "x", period = "2022")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$set_fix_values(333, names = "x", period = "2024")
  expect_output(
    mdl2$fill_mdl_data(include_frmls = TRUE, report = "minimal"),
    "Replaced a total of 4 missing/invalid values",
    fixed = TRUE
  )
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(1, 4, 333, 6))

  # Fix x (2) ----

  mdl2 <- mdl$copy()
  mdl2$set_values(1, names = "x", period = "2022")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$set_fix_values(333, names = "x", period = "2024")
  mdl2$set_values(NA, names = "x", period = "2024")
  expect_output(
    mdl2$fill_mdl_data(include_frmls = TRUE, report = "minimal"),
    "Replaced a total of 5 missing/invalid values",
    fixed = TRUE
  )
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(1, 4, 5, 6))

  # Problem solving implicit equation  ----
  mdl2 <- mdl$copy()
  mdl2$set_values(1, names = "x", period = "2020")
  mdl2$set_values(c(4, -16, 25, 36), names = "a", period = "2020/2023")
  expect_output(
    mdl2$fill_mdl_data(include_frmls = TRUE, report = "minimal"),
    paste("No convergence in 50 iterations\nin period 2021.+Replaced a total",
          "of 7 missing/invalid values")
  )
  # The implicit equation (x**2 - a = 0) has two
  # solution (+sqrt(a) and -sqrt(a)). The actual solution depends on the starting
  # value, which is the value of x in the previous year in this case.
  sign <- sign(as.numeric(mdl2$get_data(names = "x", period = "2021")))
  expect_equal(
    as.numeric(sign * mdl2$get_data(period = "2022/2023", names = "x")),
    c(5, 6)
  )
})
