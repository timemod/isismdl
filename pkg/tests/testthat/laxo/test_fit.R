context("fit for laxo model")

mdl_file <- "mdl/laxo.mdl"
mif_file <- "mdl/laxo.mif"
input_csv <- "data/input.csv"
fit_target_csv <- "data/fit_targets.csv"
rms_csv <- "data/rms_values.csv"
isis_result_csv <- "isi/fit.csv"
isis_result_ca_csv <- "isi/fit_ca.csv"

mdl_period <- regperiod_range("1993Q1/1993Q4")

# read input
input <- as.regts(read.csv(input_csv), time_column = 1)
fit_targets <- as.regts(read.csv(fit_target_csv), time_column = 1)
colnames(fit_targets) <- gsub("_fit", "", colnames(fit_targets))

# rms values
rms_data <- read.csv(rms_csv, row.names = 1, header = FALSE)
rms_values <- rms_data[, 1]
names(rms_values) <- gsub("_rms", "", tolower(rownames(rms_data)))

isis_result <- as.regts(read.csv(isis_result_csv), time_column = 1)
isis_ca_result <- as.regts(read.csv(isis_result_ca_csv), time_column = 1)
colnames(isis_ca_result) <- gsub("_ca", "", colnames(isis_ca_result))

report <- capture_output(mdl <- compile_mdl("mdl/laxo.mdl"))

mdl$set_period(mdl_period)
mdl$set_data(input)
mdl$set_fit(fit_targets)

mdl$set_fit(regts(177.84782, start = "1993Q2"), names = "ybfexvk")
mdl$set_fit(regts(0.00489, start = "1993Q2"), names = "c____pr")

mdl$set_rms(rms_values)
# TODO: options = list(report = "none") does not work yet for the fit procedure
report <- capture_output(mdl$solve(options = list(report = "none")))

dif <- tsdif(mdl$get_data()[mdl_period, ], isis_result, tol = 1e-6, fun = cvgdif)

ca_names <- mdl$get_var_names(vtype = "allfrml")
dif_ca <- tsdif(mdl$get_ca(), isis_ca_result, tol = 1e-6, fun = cvgdif)

test_that("Testing get_fit", {
    fit_ref <- fit_targets
    fit_ref["1993Q2", c("ybfexvk", "c____pr")] <- c(177.84782, 0.00489)
    expect_identical(mdl$get_fit()[, colnames(fit_ref)], fit_ref)
})

test_that("Comparing the results", {
    expect_identical(dif$missing_names1, character(0))
    expect_identical(dif$missing_names2, character(0))
    expect_identical(dif$difnames, character(0))
    expect_identical(dif_ca$missing_names1, character(0))
    expect_identical(dif_ca$missing_names2, character(0))
    expect_identical(dif_ca$difnames, character(0))
})

