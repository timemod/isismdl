library(utils)
library(isismdl)
library(testthat)

context("test for complex model 1")

period <- as.period_range("476")
mdl_filename <- "mdl/conditional1.mdl"
mdl_subst_filename <- "mdl/conditional1_subst.mdl"

flags_1 <- c("no_flag", "no_flag" , "one"    , "one"     , "two"    , "two" ,
             "two")
flags_2 <- c("no_flag", "times_10", "no_flag", "times_10", "no_flag", "times_10",
            "times_100")

test_that("the parse flags are handles correctly", {
  results <- numeric(length(flags_1))
  for (i in seq_along(flags_1)) {
    parse_options <- list(flags = c(flags_1[i], flags_2[i]))
    outp <- capture.output(mdl <- isis_mdl(mdl_filename, period = period,
                                           parse_options = parse_options))
    mdl$solve(options = list(report = "none"))
    results[i] <- as.numeric(mdl$get_data(names = "x_copy"))
  }
  expected_results <- c(3, 30, 1, 10, 2000, 20, 200)
  expect_equal(results, expected_results)
})

test_that("convert_mdl_file also handles flags correctly", {
  parse_options <- list(flags = c("two", "times_100"))
  outp <- capture.output(convert_mdl_file(mdl_filename, mdl_subst_filename,
                                  parse_options = parse_options))
  outp <- capture.output(mdl <- isis_mdl(mdl_subst_filename, period = period,
                                         parse_options = parse_options))
  mdl$solve(options = list(report = "none"))
  expected_data <- regts(matrix(200, ncol = 2), period = "476",
                         names = mdl$get_var_names())
  expect_equal(mdl$get_data(), expected_data)
})
