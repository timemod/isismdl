library(isismdl)
library(testthat)

rm(list = ls())
update <- FALSE

context("run_eqn_2")

source("../tools/read_mrf.R")

Sys.setlocale("LC_COLLATE", "C")

mdl_file <- "mdl/test_run_eqn_2.mdl"

solve_per <- period_range(2021, 2023)
mdl <- isis_mdl(mdl_file, silent = TRUE, period = solve_per)
