library(testthat)
library(isismdl)

rm(list = ls())

report_opt <- "none"
fit_dbgopt <- "noprijac"
silent <- TRUE

# prepare rms values and fit targets
rms_values <- c(c = 5.0, t = 2, i = 21, md = 2)
i <- regts(200, start = "2015Q2")
y <- regts(c(990, NA, 1010), start = "2015Q2")
fit_targets <- cbind(y, i)

mdl_bas <- read_mdl("islm_model.ismdl", silent = silent)
mdl_bas$set_period("2015Q2/2016Q1")
mdl_bas$set_solve_options(report = report_opt)
mdl_bas$set_fit_options(dbgopt = fit_dbgopt)

mdl_bas$set_fit(fit_targets)
mdl_bas$set_rms(rms_values)

data_init <- mdl_bas$get_data()

jac_2015Q2 <- matrix(
  c(
    8.031471485207,   33.7261256521,   -2.1502843546409, -2.0240724468840,
    0.321258859408,  22.3490450261,    -0.0860113741857, -0.0809628978755
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("y", "i"), c("c", "i", "md", "t"))
)

jac_2015Q4 <- matrix(
  c(8.04755926228, 33.7939478805, -2.13717634834, -2.02812064809),
  nrow = 1,
  dimnames = list("y", c("c", "i", "md", "t"))
)

test_that("fit options", {

  mdl <- mdl_bas$copy()
  mdl$solve(fit_options = list(store_jac = TRUE))
  expect_equal(mdl$get_fit_jac(), jac_2015Q4)

  mdl <- mdl_bas$copy()
  mdl$solve(period = "/2015Q3", fit_options = list(store_jac = TRUE))
  expect_equal(mdl$get_fit_jac(), jac_2015Q2)

  # Now perform a lazy fit procedure. Since there are no iterations,
  # the fit Jacobian is never computed.
  mdl$solve(fit_options = list(zealous = FALSE))
  expect_null(mdl$get_fit_jac())

})

test_that("fixed rms", {
  mdl <- mdl_bas$copy()
  mdl$set_fit_options(store_jac = TRUE)
  mdl$set_fix_values(202, names = "t", period = "2015q2")

  jac_2015Q2_2 <- matrix(
    c(
      10.33518600700, 43.3949539801, -2.76392155176,
      0.41340744028, 22.7357981592, -0.11055686207
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("y", "i"), c("c", "i", "md"))
  )

  jac_2015Q4_2 <- matrix(
    c(8.04755926228, 33.7939478805, -2.13717634834, -2.02812064809),
    nrow = 1,
    dimnames = list("y", c("c", "i", "md", "t"))
  )

  mdl$solve(period = "2015Q2")
  expect_equal(mdl$get_fit_jac(), jac_2015Q2_2, tolerance = 1e-6)

  # also check combination fit and solve jac
  mdl$solve(period = "2015Q3/2015Q4",
            options = list(store_jac = TRUE))
  expect_equal(mdl$get_fit_jac(), jac_2015Q4_2, tolerance = 1e-6)

  solve_jac <- jac_2015Q2_2 <- matrix(
    c(
      -0.119298142223, 0.0010997645428,
      16.649832500041, 0.4685999999999
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("r", "y"), c("r", "y"))
  )
  expect_equal(mdl$get_solve_jac(), solve_jac)

  mdl$solve(period = "2015Q3/2015Q4")
  expect_equal(mdl$get_fit_jac(), jac_2015Q4_2, tolerance = 1e-6)

  mdl$solve(period = "2015Q3/2015Q4", fit_options = list(store_jac = FALSE))
  expect_null(mdl$get_fit_jac())
})
