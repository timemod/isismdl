library(isismdl)
library(testthat)

rm(list = ls())

mdl_file <- "mdl/implicit_example1.mdl"
mdl <- isis_mdl(mdl_file, period = "2020/2025", silent = TRUE)

test_that("solve", {
  mdl2 <- mdl$copy()
  mdl2$set_values(c(1, 2, 3), names = "x", period = "2022/2024")
  mdl2$set_values(c(4, 16, 25, 36), names = "a", period = "2022/2025")
  mdl2$solve(options = list(erropt = "silent", report = "none"))
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               c(2, 4, 5, NA))

  mdl2$set_ca_values(1:4, names = "x", period = "2022/2025")
  mdl2$solve(options = list(erropt = "silent", report = "none"))
  expect_equal(as.numeric(mdl2$get_data(period = "2022/2025", names = "x")),
               sqrt(c(3, 14, 22, NA)))
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

test_that("fill_mdl_data", {
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
})
