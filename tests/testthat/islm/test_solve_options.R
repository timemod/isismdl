context("solve options for ISLM model")

library(utils)

mif_file <- "mdl/islm.mif"
input_file <- "input/input_mws.RData"

load(input_file)

islm_model <- MacroModel$new(mif_file)

test_that("get_solve_options / set_solve_options", {
    default_opts <- islm_model$get_solve_options()
    expect_equal_to_reference(default_opts, file = "data/default_solve_opts.Rds")
    opts <- default_opts
    opts["mode"] <- "reschk"
    opts["cnmtrx"] <- 0.7
    opts["xupdate"] <- "lastval"
    expect_identical(islm_model$set_solve_options(opts), islm_model)
    expect_identical(islm_model$get_solve_options(), opts)

    islm_model$set_solve_options(list(dbgopt = c("priscal", "prifb")))
    expect_identical(islm_model$get_solve_options()[["dbgopt"]],
                     c("prifb", "noprild", "noprijac", "noprinoconv",
                       "noprinotconvl", "priscal"))
})
