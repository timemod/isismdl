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

  ifn_mdl$set_ftrelax(0.25)

  emsg <- "value should be a single numerical value"
  expect_error(ifn_mdl$set_ftrelax("x"), emsg)
  expect_error(ifn_mdl$set_ftrelax(1:2), emsg)
  expect_error(
    ifn_mdl$set_ftrelax(0.15, names = "rhox"),
    "Variable 'rhox' is not an endogenous lead.",
    fixed = TRUE
  )

  emsg <- "The following variables are not endogenous leads: 'A' and 'ETA'."
  expect_error(
    ifn_mdl$set_ftrelax(0.77, names = c("A", "ETA")),
    emsg,
    fixed = TRUE
  )


  names <-  c("a", "etaa", paste0("abcdefhijilklmn", 1:8))
  emsg <- paste0(
    "The following variables are not endogenous leads: 'etaa', ",
    "'abcdefhijilklmn1',\n    'abcdefhijilklmn2', 'abcdefhijilklmn3', ",
    "'abcdefhijilklmn4',\n    'abcdefhijilklmn5', 'abcdefhijilklmn6', ",
    "'abcdefhijilklmn7' and\n    'abcdefhijilklmn8'."
  )
  expect_error(
    ifn_mdl$set_ftrelax(0.77, names = names),
    emsg,
    fixed = TRUE
  )

  # The relaxation factor should not have been modified
  expected <- c(a = 0.25, eta = 0.25, lambda = 0.25, mzk = 0.25, px = 0.25,
                rho = 0.25)
  expect_identical(ifn_mdl$get_ftrelax(), expected)
})
