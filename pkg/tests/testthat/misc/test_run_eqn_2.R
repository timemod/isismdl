rm(list = ls())
library(isismdl)
library(testthat)

# Test correct handling of fixed variables for update_mode updval.
# This job did not run correctly in versions before 2.2.3 (the bug
# was fixed in version 2.2.3)

rm(list = ls())

mdl_file <- "mdl/test_run_eqn_2.mdl"
solve_per <- "2021/2022"

test_that("run_eqn correctly handles fixed variables for updval", {
  mdl <- isis_mdl(mdl_file, silent = TRUE, period = solve_per)
  mdl$set_values(1, names = "x", period = "2022")
  mdl$set_fix_values(10, names = "y", period = "2021")
  mdl$set_values(99, names = "y", period = "2022")

  mdl$run_eqn(update_mode = "updval")

  #print(mdl$get_data())

  expect_equal(mdl$get_data(names = "y")[, 1], regts(c(10, 1), period = solve_per))
})
