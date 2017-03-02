context("solve ISLM model (S4 version)")

library(utils)

capture_output(islm_model <- read_mdl_S4("islm_model_S4.rds"))

report <- capture_output(islm_model <- solve_mdl(islm_model))

isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)
dif <- tsdif(mdl_data(islm_model, period = "2015Q2/2016Q3"), isis_result,
             tol = 1e-6, fun = cvgdif)

test_that("Comparing ordinary solve for the ISLM model", {
    expect_identical(islm_model@data_period, regperiod_range("2015Q1", "2016Q3"))
    expect_identical(var_names(islm_model), colnames(isis_result))
    expect_identical(var_names(islm_model, type = "allfrml"),
                     c("c", "i", "md", "t"))
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

