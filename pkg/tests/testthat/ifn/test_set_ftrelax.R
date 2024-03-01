library(isismdl)
library(testthat)
library(utils)

rm(list = ls())

update_expected <- FALSE

ifn_mdl <- read_mdl("ifn_mdl.rds", silent = TRUE)

test_that("set_ftrelax", {

  expected <- c(a = NA, eta = NA, lambda = 0.5, mzk = NA, px = NA, rho = NA)
  expect_identical(ifn_mdl$get_ftrelax(), expected)

  expect_warning({
    ifn_mdl$set_ftrelax(0.2, names = "px")
    ifn_mdl$set_ftrelax(0.3, pattern = ".a")
  },
  NA
  )
  expected <- c(a = NA, eta = 0.3, lambda = 0.3, mzk = NA, px = 0.2, rho = NA)
  expect_identical(ifn_mdl$get_ftrelax(), expected)

  ifn_mdl$set_ftrelax(0.4, pattern = "(l|m)", names = "a")

  expected <- c(a = 0.4, eta = 0.3, lambda = 0.4, mzk = 0.4, px = 0.2, rho = NA)
  expect_identical(ifn_mdl$get_ftrelax(), expected)

  expect_warning(
    ifn_mdl$set_ftrelax(0.7),
    NA
  )
  expected[] <- 0.7
  expect_identical(ifn_mdl$get_ftrelax(), expected)

  expect_warning(
    ifn_mdl$set_ftrelax(NA),
    NA
  )
  expected[] <- NA
  expect_identical(ifn_mdl$get_ftrelax(), expected)

  ifn_mdl$set_ftrelax(0.99, pattern = "xyz")
  expect_identical(ifn_mdl$get_ftrelax(), expected)

  ifn_mdl$set_ftrelax(0.25, names = c("a", "eta"))
  expected <- c(a = 0.25, eta = 0.25, lambda = NA, mzk = NA, px = NA, rho = NA)
  expect_identical(ifn_mdl$get_ftrelax(), expected)
})

test_that("errors", {
  emsg <- "value should be a single numerical value"
  expect_error(ifn_mdl$set_ftrelax("x"), emsg)
  expect_error(ifn_mdl$set_ftrelax(1:2), emsg)

  expect_warning(
    expect_error(
      ifn_mdl$set_ftrelax(0.15, names = "rhox"),
      "incorrect variable name(s) encountered. See warning(s)",
      fixed = TRUE
    ),
    '"rhox" is not an endogenous lead.',
    fixed = TRUE
  )
})
