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
  # mdl2 <- mdl$copy()
  # mdl2$set_values(1, names = "x", period = "2021")
  # mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  # mdl2$set_ca_values(c(1, 2, 3, NA), names = "x", period = "2022/2025")
  # mdl2$solve(period = "2022/2025", options = list(erropt = "cont", report = "period"))
  # print(mdl2$get_data(period = "2022/2025", names = "x"))
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
  mdl2 <- mdl$copy()
  mdl2$set_values(c(1, 2, 3), names = "x", period = "2022/2024")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$run_eqn(names = "x", period = "2022/2025")
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(2, 4, 5, 6))

  mdl2$set_ca_values(1:4, names = "x", period = "2022/2025")
  mdl2$solve(options = list(erropt = "silent", report = "none"))
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               sqrt(c(3, 14, 22, 32)))
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
})
