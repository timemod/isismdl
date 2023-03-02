library(isismdl)
library(testthat)


rm(list = ls())
capture_output(islm_model <- read_mdl("islm_model.ismdl"))

i <- regts(200, start = '2015Q2')
c <- regts(c(600, NA, 600), start = '2015Q2')
fix <- cbind(i, c)
ts_labels(fix) <- c("investment", "consumption")
islm_model$set_fix(fix)

report <- capture_output(islm_model$solve())
#print(islm_model$get_data())

isis_result <- as.regts(read.csv("isi/fix.csv"), time_column = 1)
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)
#print(dif)
test_that("Testing get_fix", {
    expect_identical(islm_model$get_fix(), fix[ , c("c", "i")])
})

test_that("Comparing solve with fix variables for the ISLM model", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

#  now unfix the variables, set CAs to zero and solve again
islm_model$set_fix_values(NA, pattern = ".*")
islm_model$set_ca_values(0)
report <- capture_output(islm_model$solve())
isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)
dif <- tsdif(islm_model$get_data(period = "2015Q2/2016Q3"), isis_result,
             tol = 1e-6, fun = cvgdif)
#print(dif)

test_that("Comparing solve after unfixing variables for the ISLM model", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})
