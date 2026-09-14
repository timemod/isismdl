library(testthat)
library(isismdl)
library(tibble)

rm(list = ls())

test_that("fbstart (a)", {

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

  expect_output(
    expect_warning(
      mdl_prev$solve(options = list(report = "period")),
      NA
    ),
    paste0("Warning - previous period initial feedback values\n",
           "in first period of simulation not available")
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
  result <- as_tibble(regts::as_data_frame(result))

  expect_equal(
    result,
    tribble(
      ~period, ~mdl, ~mdl_curr, ~mdl_prev, ~mdl_curifok, ~mdl_previfok,
      "2021",    1,         2,         2,            2,             2,
      "2022",   -1,        -2,         2,           -2,             2,
      "2023",   NA,        NA,         2,           -2,             2,
      "2024",    1,         1,         2,            2,             2,
      "2025",   -1,        -1,         2,           -2,             2
    )
  )
})

test_that("fbstart (b)", {

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
    data <- round(get(name)$get_data())
    colnames(data) <- name
    result <- cbind(result, data)
  }
  result <- as_tibble(regts::as_data_frame(result))

  expect_equal(
    result,
    tribble(
      ~period, ~mdl, ~mdl_curr, ~mdl_prev, ~mdl_curifok, ~mdl_previfok,
      "2021",   NA,        NA,        NA,           NA,            NA,
      "2022",   -1,        -2,        -1,           -2,            -2,
      "2023",   NA,        NA,        NA,           -2,            -2,
      "2024",    1,         1,         1,            2,            -2,
      "2025",   -1,        -1,        -1,           -2,            -2
    )
  )
})

test_that("fbstart (with lag)", {

  model_text <- "ident x = x + x**2 - 4 + 0 * dummy[-1];"
  mdl <- isis_mdl(model_text = model_text, period = "2022/2025",
                  silent = TRUE)
  mdl$set_values(1, names = "dummy")
  mdl$set_values(c(NA, -1, NA, 1, -1), names = "x")
  mdl$set_solve_options(report = "none")

  expect_warning(
    mdl_curr <- mdl$copy()$solve(),
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
    data <- get(name)$get_data(names = "x")
    colnames(data) <- name
    result <- cbind(result, data)
  }
  result <- as_tibble(regts::as_data_frame(result))

  expect_equal(
    result,
    tribble(
      ~period, ~mdl, ~mdl_curr, ~mdl_prev, ~mdl_curifok, ~mdl_previfok,
      "2021",   NA,        NA,        NA,           NA,            NA,
      "2022",   -1,        -2,        -1,           -2,            -2,
      "2023",   NA,        NA,        NA,           -2,            -2,
      "2024",    1,         1,         1,            2,            -2,
      "2025",   -1,        -1,        -1,           -2,            -2
    )
  )
})
