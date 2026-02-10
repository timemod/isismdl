library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

period <- as.period_range("476")
mdl_filename <- "mdl/conditional1.mdl"
mdl_subst_filename <- "mdl/conditional1_subst.mdl"

flags_1 <- c("no_flag", "no_flag" , "one"    , "one"     , "two"    , "two" ,
             "two")
flags_2 <- c("no_flag", "times_10", "no_flag", "times_10", "no_flag", "times_10",
            "times_100")

test_that("the parse flags are handled correctly", {
  results <- numeric(length(flags_1))
  for (i in seq_along(flags_1)) {
    parse_options <- list(flags = c(flags_1[i], flags_2[i]))
    mdl <- isis_mdl(model_file = mdl_filename, period = period,
                    parse_options = parse_options, silent = TRUE)
    mdl$solve(options = list(report = "none"))
    results[i] <- as.numeric(mdl$get_data(names = "x_copy"))
  }
  expected_results <- c(3, 30, 1, 10, 2000, 20, 200)
  expect_equal(results, expected_results)
})

test_that("convert_mdl_file also handles flags correctly", {
  parse_options <- list(flags = c("two", "times_100"))
  convert_mdl_file(model_file = mdl_filename, 
                   output_file = mdl_subst_filename,
                   parse_options = parse_options)
  expect_known_output(cat(paste(readLines(mdl_subst_filename), collapse = "\n")),
                      file = "expected_output/conditional1_subst.mdl",
                      update = update_expected, print = TRUE)
  mdl_subst <- isis_mdl(model_file = mdl_subst_filename, period = period,
                        parse_options = parse_options, silent = TRUE)
  mdl_subst$solve(options = list(report = "none"))
  expected_data <- regts(matrix(200, ncol = 2), period = "476",
                         names = mdl_subst$get_var_names())
  expect_equal(mdl_subst$get_data(), expected_data)
})

test_that("test get_text", {
  parse_options <- list(flags = c("two", "times_100"))
  mdl <- isis_mdl(model_file = mdl_filename, period = period,
                  parse_options = parse_options, silent = TRUE)
  mdl_text <- mdl$get_text()
  # for using expect_known_output we have to remove the carriage return
  expect_known_output(cat(gsub("\r", "", mdl_text)),
                      file = "expected_output/conditional1_text.mdl",
                      update = update_expected, print = TRUE)
  mdl_tmp <- tempfile("isismdl_test_", fileext = ".mdl")
  writeLines(mdl_text, mdl_tmp)
  mdl_test <- isis_mdl(mdl_tmp, silent = TRUE)
  expect_identical(mdl$get_var_names(), mdl_test$get_var_names())
  expect_identical(mdl$get_dep_struct(), mdl_test$get_dep_struct())
  expect_identical(mdl$get_text(), mdl_text)
})
