context("fit for ISLM model (S4 version)")

capture_output(islm_model <- read_mdl_S4("islm_model_S4.rds"))

i <- regts(200, start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fit_targets <- cbind(y, i)

islm_model <- set_fit(islm_model, fit_targets)
islm_model <- set_rms(islm_model, values = c(c = 5.0, i = 21, md = 2, t = 2))

report <- capture_output(islm_model <- solve_mdl(islm_model))

isis_result <- as.regts(read.csv("isi/fit.csv"), time_column = 1)
dif <- tsdif(mdl_data(islm_model, period = "2015Q2/2016Q3"), isis_result,
             tol = 1e-6, fun = cvgdif)

test_that("Testing get_fit", {
    expect_identical(fit_targets(islm_model), fit_targets)
})

test_that("Comparing the results of solve", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

#  now remove fit targets and solve again
islm_model <- set_fit_values(islm_model, NA)

report <- capture_output(islm_model <- solve_mdl(islm_model))
dif <- tsdif(mdl_data(islm_model, period = "2015Q2/2016Q3"), isis_result,
             tol = 1e-6, fun = cvgdif)

test_that("Comparing solve after removing the fit targets", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

# now also set all CAs to zero
islm_model <- set_ca_values(islm_model, 0)
report <- capture_output(islm_model <- solve_mdl(islm_model))
isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)
dif <- tsdif(mdl_data(islm_model, period = islm_model@period), isis_result,
             tol = 1e-6, fun = cvgdif)
#print(dif)

test_that("Comparing solve after removing fit targets and setting the
          CAs to 0", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})
