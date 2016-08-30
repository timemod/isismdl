context("solve ISLM model")

library(utils)

mif_file <- "mdl/islm.mif"

islm_model <- IsisMdl$new(mif_file)
islm_model$set_mws(islm_input_mws)

report <- capture_output(islm_model$solve())
#print(islm_model$get_data())

isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)

test_that("Comparing ordinary solve for the ISLM model", {
    expect_identical(islm_model$model_data_period, regperiod_range("2015Q1", "2016Q3"))
    expect_identical(islm_model$get_variable_names(), colnames(isis_result))
    expect_identical(islm_model$get_variable_names(vtype = "allfrml"),
                     c("c", "i", "md", "t"))
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

