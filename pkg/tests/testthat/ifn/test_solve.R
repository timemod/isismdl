library(isismdl)
library(testthat)
library(utils)

rm(list = ls())

update_expected <- FALSE

context("solve IFN model")
capture_output(ifn_mdl <- read_mdl("ifn_mdl.rds"))

isis_result <- as.regts(read.csv("isi/solve.csv"), time_column = 1)
mdl_per <- ifn_mdl$get_period()

test_that("ftrelax correctly read from file", {
  res_correct <- c(a = NA, eta = NA, lambda = 0.5, mzk = NA, px = NA, rho = NA)
  expect_identical(ifn_mdl$get_ftrelax(), res_correct)
})

test_that("get_var_names/get_endo_names for (endogenous) leads", {

  all_leads <- c("a", "eta", "gpx", "groei", "lambda", "mzk", "px", "rho",
                 "tc")
  endo_leads <- c("a", "eta", "lambda", "mzk", "px", "rho")
  expect_identical(ifn_mdl$get_var_names(type = "leads"), all_leads)
  expect_identical(ifn_mdl$get_endo_names(type = "leads"), endo_leads)
  expect_warning(
    expect_identical(ifn_mdl$get_endo_names(type = "endolead"), endo_leads),
    "Type 'endolead' is obsolete and has been  replaced by 'leads'\\.")
  expect_identical(intersect(all_leads, ifn_mdl$get_endo_names()), endo_leads)
})

test_that("solve options read from file", {
  options_set <- list(xmaxiter = 1500, ratreport = "iter",
                      report = "minimal", ratreport_rep = 10,
                      ratfullreport_rep = 50)
  expect_equal(ifn_mdl$get_solve_options()[names(options_set)], options_set)
})

test_that("model administration correct", {
  expect_identical(mdl_per, period_range("2y", "100y"))
  expect_identical(ifn_mdl$get_data_period(), period_range("1y", "101y"))
  expect_identical(ifn_mdl$get_var_names(), colnames(isis_result))
})

test_that("solve result almost identical to Isis result", {
  ifn_mdl2 <- ifn_mdl$copy()
  ifn_mdl2$solve(options = list(report = "none"))
  dif <- tsdif(ifn_mdl2$get_data(period = mdl_per), isis_result, tol = 1e-6,
               fun = cvgdif)
  expect_identical(dif$missing_names1, character(0))
  expect_identical(dif$missing_names2, character(0))
  expect_identical(dif$difnames, character(0))
})

test_that("warning when Fair-Taylor has not converged", {
  ifn_mdl2 <- ifn_mdl$copy()
  expect_warning(ifn_mdl2$solve(options = list(xmaxiter = 10,
                                               report = "none")),
                           "Fair-Taylor has not converged")
})

test_that("errors", {
 ifn_mdl2 <- ifn_mdl$copy()
 expect_error(ifn_mdl2$solve(options = list(xmaxiter = 0, report = "none")),
              "xmaxiter should be larger than 0")
})

test_that("get_endo_names with inactive equations", {
  ifn_mdl2 <- ifn_mdl$copy()
  ifn_mdl2$set_eq_status("inactive", names = c("eta", "px"))
  all_leads <- c("a", "eta", "gpx", "groei", "lambda", "mzk", "px", "rho",
                 "tc")
  endo_leads <- c("a", "eta", "lambda", "mzk", "px", "rho")
  expect_identical(ifn_mdl2$get_var_names(type = "leads"), all_leads)
  expect_identical(ifn_mdl2$get_endo_names(type = "leads"), setdiff(endo_leads,
                                                                   c("eta", "px")))
  expect_identical(ifn_mdl2$get_endo_names(type = "leads", status = "all"),
                   endo_leads)
  expect_identical(ifn_mdl2$get_endo_names(type = "leads", status = "inactive"),
                   c("eta", "px"))

  expect_error(ifn_mdl2$set_eq_status("active", names = all_leads),
               'The following names are no equations: "gpx", "groei", "tc".',
               fixed = TRUE)

  ifn_mdl2$set_eq_status("active", names = endo_leads)
  expect_identical(ifn_mdl2$get_endo_names(type = "leads"), endo_leads)
  expect_identical(ifn_mdl2$get_endo_names(type = "leads", status = "all"),
                   endo_leads)
  expect_identical(ifn_mdl2$get_endo_names(type = "leads", status = "inactive"),
                   character(0))
})

test_that("test get_text", {
  mdl_text <- ifn_mdl$get_text()
  # for using expect_known_output we have to remove the carriage return
  expect_known_output(cat(gsub("\r", "", mdl_text)),
                      file = "expected_output/ifn_text.mdl",
                      update = update_expected, print = TRUE)
  mdl_tmp <- tempfile("isismdl_test_", fileext = ".mdl")
  writeLines(mdl_text, mdl_tmp)
  mdl_test <- isis_mdl(mdl_tmp, silent = TRUE)
  expect_identical(ifn_mdl$get_var_names(), mdl_test$get_var_names())
  expect_identical(ifn_mdl$get_dep_struct(), mdl_test$get_dep_struct())
  mdl_text_test <- mdl_test$get_text()
  # mdl_text_test contains one extra line ending (I do not understand why),
  # therefore remove the final line ending
  mdl_text_test <- sub("(\r?\n)$", "", mdl_text_test)
  expect_identical(mdl_text, mdl_text_test)
})

test_that("get_dep_struct", {
  expect_known_output(ifn_mdl$get_dep_struct(),
                      file = "expected_output/ifn.dep",
                      update = update_expected, print = TRUE)
})

