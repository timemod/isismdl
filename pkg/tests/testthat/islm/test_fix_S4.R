context("fix for ISLM model (S4 version)")

capture_output(islm_model <- read_mdl_S4("islm_model_S4.rds"))

i <- regts(200, start = '2015Q2')
c <- regts(c(600, NA, 600), start = '2015Q2')
fix <- cbind(i, c)

islm_model <- set_fix(islm_model, fix)

report <- capture_output(islm_model <- solve_mdl(islm_model))

isis_result <- as.regts(read.csv("isi/fix.csv"), time_column = 1)
dif <- tsdif(mdl_data(islm_model, period = "2015Q2/2016Q3"), isis_result,
             tol = 1e-6, fun = cvgdif)
#print(dif)
test_that("Testing get_fix", {
    expect_identical(fix_values(islm_model), fix)
})

test_that("Comparing solve with fix variables for the ISLM model", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

#  now unfix the variables, set CAs to zero and solve again
islm_model <- set_fix_values(islm_model, NA, pattern = ".*")
islm_model <- set_ca_values(islm_model, 0)
report <- capture_output(islm_model <- solve_mdl(islm_model))

isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)
dif <- tsdif(mdl_data(islm_model, period = "2015Q2/2016Q3"), isis_result,
             tol = 1e-6, fun = cvgdif)
#print(dif)

test_that("Comparing solve after unfixing variables for the ISLM model", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})
