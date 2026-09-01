library(testthat)
library(isismdl)
library(tibble)

rm(list = ls())

update_expected <- FALSE

test_that("fbstart (1)", {

  model_text <- "ident x = x + x**2 - 4;"
  mdl <- isis_mdl(model_text = model_text, period = "2021/2025",
                  silent = TRUE)
  mdl$set_values(c(1, -1, NA, 1, -1), names = "x")
  mdl$set_solve_options(report = "none")

  mdl_curr <- mdl$copy()

  # check if explicitly specified fbstart option results in the same model
  mdl_curr_2 <- mdl$copy()$set_solve_options(fbstart = "current")
  expect_true(isTRUE(all.equal(mdl_curr, mdl_curr_2)))

  mdl_prev <-  mdl$copy()$set_solve_options(fbstart = "previous")
  expect_false(isTRUE(all.equal(mdl_curr, mdl_prev)))

  expect_warning(
    mdl_curr$solve(),
    "Simulation not possible",
    fixed = TRUE
  )

  expect_warning(
    mdl_prev$solve(),
    NA
  )

  expect_warning(
    mdl_curifok <-  mdl$copy()$set_solve_options(fbstart = "curifok")$solve(),
    NA
  )

  expect_warning(
    mdl_previfok <-  mdl$copy()$set_solve_options(fbstart = "previfok")$solve(),
    NA
  )

  result <- NULL
  mdl_names <- c("mdl", "mdl_curr", "mdl_prev", "mdl_curifok", "mdl_previfok")
  for (name in mdl_names) {
    data <- get(name)$get_data()
    colnames(data) <- name
    result <- cbind(result, data)
  }

  expect_known_value(
    result,
    file = "expected_output/fbstart_1.rds",
    update = update_expected
  )
})

test_that("fbstart (2)", {

  model_text <- "ident x = x + x**2 - 4;"
  mdl <- isis_mdl(model_text = model_text, period = "2021/2025",
                  silent = TRUE)
  mdl$set_values(c(NA, -1, NA, 1, -1), names = "x")
  mdl$set_solve_options(report = "none")
  mdl$set_period("2022/2025")

  expect_warning(
    mdl_curr <- mdl$copy()$solve()
  )
  expect_warning(
    mdl_prev <-  mdl$copy()$set_solve_options(fbstart = "previous")$solve()
  )

  expect_warning(
    mdl_curifok <-  mdl$copy()$set_solve_options(fbstart = "curifok")$solve(),
    NA
  )

  expect_warning(
    mdl_previfok <-  mdl$copy()$set_solve_options(fbstart = "previfok")$solve(),
    NA
  )

  result <- NULL
  mdl_names <- c("mdl", "mdl_curr", "mdl_prev", "mdl_curifok", "mdl_previfok")
  for (name in mdl_names) {
    data <- get(name)$get_data()
    colnames(data) <- name
    result <- cbind(result, data)
  }

  expect_known_value(
    result,
    file = "expected_output/fbstart_2.rds",
    update = update_expected
  )
})
