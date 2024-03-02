library(utils)
library(isismdl)
library(testthat)

rm(list = ls())


mdl <- read_mdl("islm_model_solved.ismdl", silent = TRUE)

new_data <-  mdl$get_data()
new_data["2015Q3", "g"] <- NA
new_data["2016Q2", "t"] <- NA

test_that("set_data update mode upd", {
  mdl2 <- mdl$clone(deep = TRUE)
  mdl2$set_data(new_data)
  expect_equal(mdl2$get_data(), new_data)

  new_data2 <- cbind(new_data, x = 2)
  mdl3 <- mdl$copy()
  expect_warning(mdl3$set_data(new_data2, upd_mode = "upd", name_err = "warn"),
                 "'x' is not a model variable")
  expect_silent(mdl3$set_data(new_data2, upd_mode = "upd"))
  new_data3 <- cbind(new_data, x = 2, z = 2)
  expect_error(mdl3$set_data(new_data3, upd_mode = "upd", name_err = "stop"),
               "The following names are not model variables: 'x' and 'z'.")

  expect_silent(mdl3$set_data(new_data2, upd_mode = "upd", name_err = "silent"))
  expect_equal(mdl3$get_data(), new_data)


  names <-  c("c", "etaa", paste0("abcdefhijilklmn", 1:8))
  new_data <- regts(matrix(555, ncol = length(names)), start = "2016Q1",
                           names = names)
  msg <- paste0(
    "The following names are not model variables: 'etaa', ",
    "'abcdefhijilklmn1',\n    'abcdefhijilklmn2', 'abcdefhijilklmn3', ",
    "'abcdefhijilklmn4',\n    'abcdefhijilklmn5', 'abcdefhijilklmn6', ",
    "'abcdefhijilklmn7' and\n    'abcdefhijilklmn8'."
  )
  data_before <- mdl3$get_data()
  expect_error(
    mdl3$set_data(new_data, upd_mod = "upd", name_err = "stop"),
    msg
  )
  expect_equal(mdl3$get_data(), data_before)

  data_before <- mdl3$get_data()
  expect_warning(
    mdl3$set_data(new_data, upd_mod = "upd", name_err = "warn"),
    msg
  )
  data_expected <- data_before
  data_expected[get_period_range(new_data), "c"] <- 555
  expect_equal(mdl3$get_data(), data_expected)
})

test_that("set_data update mode updval", {
  mdl2 <- mdl$clone(deep = TRUE)

  # create a data with duplicate names
  new_data$x <- new_data$t * 2
  colnames(new_data)[ncol(new_data)] <- "t"

  expect_warning(mdl2$set_data(new_data, upd_mode = "updval"),
                 paste("Data contains duplicate names. The first column is",
                      "used.\nThe duplicated names are: t."))
  expect_equal(mdl2$get_data(), mdl$get_data())
})

test_that("errors", {
  ydat <- regts(matrix(0, ncol = 2, nrow =2), names = c("i", "c"),
                 period = "2017/2018")
  msg <- paste("The frequency of data does not agree with the data period",
               "2015Q1/2016Q3.")
  expect_error(mdl$set_data(ydat), msg)
  msg <- "Argument data is not a timeseries object"
  expect_error(mdl$set_data(3), msg)
})

test_that("set_data integer / logical values", {
  mdl2 <- mdl$copy()

  per <- mdl$get_period()
  mdl2$set_data(regts(1L, period = per), names = "y")

  expected_result1 <- regts(matrix(1, ncol = 1), period = per, names = "y")
  expect_equal(mdl2$get_data(names = "y", period = per), expected_result1,
               check.attributes = FALSE)

  mdl2$set_data(regts(NA, period = per), names = "y")
  expected_result2 <- regts(matrix(NA_real_, ncol = 1), period = per,
                            names = "y")
  expect_equal(mdl2$get_data(names = "y", period = per), expected_result2,
               check.attributes = FALSE)

})
