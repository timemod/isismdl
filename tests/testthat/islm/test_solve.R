mif_file <- "mdl/islm.mif"
input_file <- "input/input.RData"

load(input_file)

islm_model <- read_mdl(mif_file)
islm_model$set_period(model_period)
islm_model$set_data(input)

result <- islm_model$solve()
#print(result)
#print(islm_model$get_data())

isis_result <- as.regts(read.csv("isi/solve.csv"), index.column = 1,
                        FUN = zoo::as.yearqtr, format = "%Y.%qQ")
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)

test_that("Comparing ordinary solve for the ISLM model", {
    expect_identical(islm_model$model_data_period, regperiod_range("2015Q1", "2016Q3"))
    expect_identical(islm_model$get_variable_names(), colnames(isis_result))
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

