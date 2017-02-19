context("fit for ISLM model")

capture_output(islm_model <- read_mdl("islm_mdl.rds"))

i <- regts(200, start = '2015Q2')
y <- regts(c(990, NA, 1010), start = '2015Q2')
fit_targets <- cbind(y, i)

islm_model$set_fit(fit_targets)
islm_model$set_rms(c(c = 5.0, i = 21, md = 2, t = 2))

report <- capture_output(islm_model$solve())

isis_result <- as.regts(read.csv("isi/fit.csv"), time_column = 1)
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)

test_that("Testing get_fit", {
    expect_identical(islm_model$get_fit(), fit_targets[, c("i", "y")])
}) 

test_that("Comparing the results of solve", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

#  now remove fit targets and solve again
fit <- islm_model$get_data()[islm_model$get_period(), ]
fit[] <- NA
islm_model$set_fit(fit)

report <- capture_output(islm_model$solve())
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)

test_that("Comparing solve after removing the fit targets", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})

# now also set all CAs to zero
ca <- islm_model$get_data()[islm_model$get_period(), ]
ca[] <- 0
islm_model$set_ca(ca)
report <- capture_output(islm_model$solve())
isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)
dif <- tsdif(islm_model$get_data()[islm_model$get_period(), ], isis_result,
             tol = 1e-6, fun = cvgdif)
#print(dif)

test_that("Comparing solve after removing fit targets and setting the
          CAs to 0", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})
