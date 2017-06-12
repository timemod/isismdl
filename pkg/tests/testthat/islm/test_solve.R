context("solve ISLM model")

library(utils)

capture_output(islm_model <- read_mdl("islm_model.rds"))

report <- capture_output(islm_model$solve())

isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)

dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)
test_that("Comparing ordinary solve for the ISLM model", {
    expect_identical(islm_model$get_data_period(),
                     period_range("2015Q1", "2016Q3"))
    expect_identical(islm_model$get_var_names(), colnames(isis_result))
    expect_identical(islm_model$get_var_names(type = "allfrml"),
                     c("c", "i", "md", "t"))
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

