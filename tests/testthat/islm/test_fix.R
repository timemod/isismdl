mif_file <- "mdl/islm.mif"
input_file <- "input/input.RData"

load(input_file)

islm_model <- read_mdl(mif_file)
islm_model$set_period(model_period)
islm_model$set_data(input)

i <- regts(200, start = '2015Q2')
c <- regts(c(600, NA, 600), start = '2015Q2')
fix <- regts.union(i, c)

islm_model$set_fix(fix)

result <- islm_model$solve()
#print(result)
#print(islm_model$get_data())

isis_result <- as.regts(read.csv("isi/fix.csv"), index.column = 1,
                        FUN = zoo::as.yearqtr, format = "%Y.%qQ")
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)
#print(dif)

test_that("Comparing solve with fix variables for the ISLM model", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

#  now unfix the variables and solve again
fix <- islm_model$get_data()[model_period, ]
fix[] <- NA
ca <- islm_model$get_data()[model_period, ]
ca[] <- 0
islm_model$set_fix(fix)
islm_model$set_ca(ca)
islm_model$solve()

isis_result <- as.regts(read.csv("isi/solve.csv"), index.column = 1,
                        FUN = zoo::as.yearqtr, format = "%Y.%qQ")
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)
#print(dif)

test_that("Comparing solve after unfixing variables for the ISLM model", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

