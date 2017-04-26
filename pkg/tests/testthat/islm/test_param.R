context("modify parameters ISLM model")

library(utils)

capture_output(islm_model <- read_mdl("islm_model.rds"))

islm_model$set_param(list(c0 = 120, t1 = 0.25))

report <- capture_output(islm_model$solve())
#print(islm_model$get_data())

isis_result <- as.regts(read.csv("isi/param.csv"), time_column = 1)
dif <- tsdif(islm_model$get_data()["2015Q2/2016Q3", ], isis_result, tol = 1e-6,
             fun = cvgdif)

test_that("Comparing results of ordinary solve after changing parameters", {
    expect_identical(islm_model$get_data_period(), period_range("2015Q1", "2016Q3"))
    expect_identical(islm_model$get_var_names(), colnames(isis_result))
    expect_identical(islm_model$get_var_names(vtype = "allfrml"),
                     c("c", "i", "md", "t"))
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
})


test_that("get_param", {
    expect_identical(islm_model$get_param(names = "c0"), list(c0 = 120))
    expect_identical(islm_model$get_param(names = "t1"), list(t1 = 0.25))
    expect_identical(islm_model$get_param(pattern = "^c."),
                     list(c0 = 120, c1 = 0.7, c2 = 20, c3 = 0.5))
})

test_that("warnings and errors for set_param", {
    expect_error(islm_model$set_param(2), "Argument p is not a list")
    expect_error(islm_model$set_param(list(2,3)), "Argument p has no names")
    expect_warning(islm_model$set_param(list(c0 = 120, xx = c(1,2))),
                   "xx is no model parameter\\(s\\)")
    expect_error(islm_model$set_param(list(c0 = c(120, 120))),
                   "Value for parameter c0 has an incorrect length. Required length: 1. Actual length: 2")
})


