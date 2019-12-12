library(utils)
library(isismdl)
library(testthat)

context("change_data for the ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.ismdl"))

c_multipliers <- seq(0.8, 1.0, length.out = nperiod(mdl$get_data_period()))
m_additions <- seq(10, 40, length.out = 4)
new_data <- mdl$get_data()
new_data[ , "c" ] <- new_data[, "c"] * c_multipliers
new_data["2015Q3/2016Q2", c("ms", "md")] <-
  new_data["2015Q3/2016Q2", c("ms", "md")] + m_additions
new_data["2016Q2", c("y", "yd")] <- new_data["2016Q2", c("y", "yd")] * 1.1

test_that("change_data works correctly", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$change_data(function(x, fac) {x * fac}, names = "c", fac = c_multipliers)
  mdl2$change_data(function(x) {x + m_additions}, names = c("ms", "md"),
                   period = "2015Q3/2016Q2")
  mdl2$change_data(function(x) {x * 1.1}, pattern = "^y", period = "2016Q2")
  expect_equal(mdl2$get_data(), new_data)
})

test_that("change_data works correctly with timeseries input", {
  mdl2 <- mdl$copy()
  mdl2$change_data(function(x, fac) {x * fac}, names = "c", fac = c_multipliers)
  m_ts <- regts(m_additions, period = "2015Q3/2016Q2")
  mdl2$change_data(function(x) {x + m_ts}, names = c("ms", "md"),
                   period = "2015Q3/2016Q2")
  mdl2$change_data(function(x) {x * 1.1}, pattern = "^y", period = "2016Q2")
  expect_equal(mdl2$get_data(), new_data)
})

test_that("change_data works correctly with timeseries input (2)", {
  mdl2 <- mdl$copy()
  mdl2$change_data(function(x, fac) {x * fac}, names = "c", fac = c_multipliers)
  m_ts <- regts(c(-999, m_additions, 999), period = "2015Q2/2016Q3")
  expect_error(
    expect_warning(mdl2$change_data(function(x) {x + m_ts}, names = c("ms", "md"),
                                    period = "2015Q3/2016Q2"),
                   "longer object length is not a multiple of shorter object length"),
    "The function result has length 6 but should have length 1 or 4\\.")

  mdl2$change_data(function(x) {x * 1.1}, pattern = "^y", period = "2016Q2")

  mdl2$change_data(function(x) {x + m_ts["2015q3/2016q2"]}, names = c("ms", "md"),
                   period = "2015Q3/2016Q2")
  expect_equal(mdl2$get_data(), new_data)
})

test_that("change_data works correctly with timeseries input (3)", {
  mdl2 <- mdl$copy()
  mdl2$change_data(function(x, fac) {x * fac}, names = "c", fac = c_multipliers,
                   period = "2015/")
  m_ts <- regts(c(-999, m_additions, 999), period = "2015Q2/2016Q2")

  expect_error(mdl2$change_data(function(x) {x + m_ts["2015q3"]},
                                names = c("ms", "md"),
                                period = "2015Q3/2016Q2"),
               "time-series/vector length mismatch")
  for (per in as.list(seq(period("2015q3"), period("2016q2")))) {
    mdl2$change_data(function(x) {x + m_ts[per]}, names = c("ms", "md"),
                     period = per)
  }

  mdl2$change_data(function(x) {x * 1.1}, pattern = "^y", period = "2016Q2")


  msg <- paste("Specified period \\(11Q1/11Q4\\) is complete outside the",
              "data period \\(2015Q1/2016Q3\\)\\.")
  expect_warning(mdl2$change_data(function(x) {x * 1.1}, pattern = "^y",
                                  period = "11"),
                 msg)

  expect_equal(mdl2$get_data(), new_data)
})

test_that("change_data handles errors correctly", {
  f <- function(x) {x}
  mdl2 <- mdl$clone(deep = TRUE)
  msg <- "xxx is not a model variable"
  expect_error(mdl2$change_data(f, names = c("y", "xxx")), msg)
  msg <- "The variables p xxx are no model variables"
  expect_error(mdl2$change_data(f, names = c("p", "xxx")), msg)
  msg <- "Argument 'fun' is not a function"
  expect_error(mdl2$change_data(2, names), msg)
})


