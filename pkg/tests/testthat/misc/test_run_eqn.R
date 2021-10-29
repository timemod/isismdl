library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE

context("run_eqn_1")

source("../tools/read_mrf.R")

Sys.setlocale("LC_COLLATE", "C")

mdl_file <- "mdl/test_run_eqn_1.mdl"

lag_data <- regts(matrix(1:2, ncol = 2), start = 2020,
                   names = c("x1", "y1"))
lead_data <- regts(matrix(3:5, ncol = 3), start = 2025,
                   names = c("x2", "y2", "z1"))
init_data <- cbind(lag_data, lead_data)

solve_per <- period_range(2021, 2023)
mdl <- isis_mdl(mdl_file, silent = TRUE, period = solve_per,
                data = init_data)
print(mdl$get_data())

mdl$run_eqn(names = c("x1", "y1"), period = period_range(2021, 2025),
            solve_order = TRUE)
print(mdl$get_data())


mdl$run_eqn(names = c("x2", "y2", "z1"), period = period_range(2020, 2024),
            forwards = FALSE, solve_order = TRUE)
print(mdl$get_data())

#mdl_solve <- mdl$copy()
#mdl_solve$solve()
