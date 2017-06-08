context("solve IFN model")

library(utils)

capture_output(ifn_mdl <- read_mdl("ifn_mdl.rds"))

ifn_mdl$set_ftrelax(0.5, names = "lambda")
ifn_mdl$set_solve_options(xmaxiter = 1500, ratreport = "iter",
                         report = "minimal", ratreport_rep = c(10, 50))

report <- capture_output(ifn_mdl$solve())

isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)

mdl_per <- ifn_mdl$get_period()
dif <- tsdif(ifn_mdl$get_data(period = mdl_per), isis_result, tol = 1e-6,
             fun = cvgdif)
test_that("Comparing ordinary solve for the ISLM model", {
    expect_identical(ifn_mdl$get_data_period(), period_range("1y", "101y"))
    expect_identical(ifn_mdl$get_var_names(), colnames(isis_result))
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

