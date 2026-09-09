library(isismdl)
library(testthat)

rm(list = ls())

report_opt <- "none"
dbgopt <- "noprijac"
silent <- TRUE

# names of feedback variables
fb_names <- c("r", "y")

mdl_bas <- read_mdl("islm_model.ismdl", silent = TRUE)$set_period("2015Q2/2015Q3")
init_data <- mdl_bas$get_data()

# set fixed solve options
# a small value of cnmtrx enforeces a fresh jacobian calculation at each step
mdl_bas$set_solve_options(cnmtrx = 1e-12, report = report_opt, dbgopt = dbgopt)

test_that("solve options", {

  mdl <- mdl_bas$copy()

  mdl$solve()
  expect_null(mdl$get_solve_jac())

  mdl$set_data(init_data)
  mdl$solve(options = list(store_jac = TRUE))

  jac_expected <- matrix(c(-0.12170626256, 0.00111626101095,
                           16.69232228942, 0.46859999999857),
                         nrow = 2, byrow = TRUE,
                         dimnames = list(fb_names, fb_names))
  expect_equal(mdl$get_solve_jac(), jac_expected)

  # write model to a file and read this file
  ismdl_file <- tempfile("isismdl_", fileext = ".ismdl")
  mdl$write_mdl(ismdl_file, silent = silent)
  mdl_read <- read_mdl(ismdl_file, silent = silent)
  expect_equal(mdl_read$get_solve_jac(), jac_expected)

  # convergence in zero iterations -> jac reset to NULL
  mdl$solve(options = list(store_jac = TRUE))
  expect_null(mdl$get_solve_jac())

  # now also check get_fit_jac() and get_jacobian()
  expect_null(mdl$get_fit_jac())
  expect_null(mdl$get_jacobian())

  expect_false(mdl$get_solve_options()$store_jac)
})

test_that("deactivated feedback variables", {

  mdl <- mdl_bas$copy()

  mdl$set_eq_status(status = "inactive", names = fb_names)
  mdl$set_solve_options(store_jac = TRUE)
  mdl$solve()

  jac_expected <- diag(2)
  rownames(jac_expected) <- fb_names
  colnames(jac_expected) <- fb_names
  expect_equal(mdl$get_solve_jac(),
               jac_expected)

})

test_that("no feedback variables", {
  mdl <- mdl_bas$copy()
  mdl$set_eq_status(status = "inactive", names = fb_names)
  mdl$order(silent = silent)
  mdl$solve(options = list(store_jac = TRUE))
  expect_null(mdl$get_solve_jac())
})

test_that("suppress global store_jac", {
  mdl <- mdl_bas$copy()
  mdl$set_solve_options(store_jac = TRUE)
  mdl$solve(options = list(store_jac = FALSE))
  expect_null(mdl$get_solve_jac())
})
