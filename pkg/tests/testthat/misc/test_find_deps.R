library(testthat)
library(isismdl)
library(tibble)
library(igraph)

rm(list = ls())

# Define a moderate model
model_text <- "
ident y1 = 0.8 * y1(-1) + 0.2 * z1;
ident y2 = 0.5 * y1 + 0.5 * z2;
ident y3 = y2 + y1(-1);
"

period <- as.period_range("2020/2022")

test_that("find_deps basic functionality", {
  mdl <- isis_mdl(model_text = model_text, period = period, silent = TRUE)
  
  final_destinations <- tribble(
    ~var, ~period,
    "y3", "2021"
  )
  
  # y3[2021] depends on y2[2021] and y1[2020]
  # y2[2021] depends on y1[2021] and z2[2021]
  # y1[2021] depends on y1[2020] and z1[2021]
  
  # observed_data is used to stop the search
  observed_data <- tribble(
    ~var, ~period,
    "y1", "2020",
    "z1", "2021",
    "z2", "2021"
  )
  
  g <- mdl$find_deps(final_destinations = final_destinations, observed_data = observed_data)
  
  expect_s3_class(g, "igraph")
  expect_true("y3[2021]" %in% V(g)$name)
  expect_true("y2[2021]" %in% V(g)$name)
  expect_true("y1[2021]" %in% V(g)$name)
  expect_true("y1[2020]" %in% V(g)$name)
  
  # Check edges
  expect_true(are_adjacent(g, "y2[2021]", "y3[2021]"))
  expect_true(are_adjacent(g, "y1[2020]", "y3[2021]"))
  expect_true(are_adjacent(g, "y1[2021]", "y2[2021]"))
})

test_that("find_deps endo checks", {
  mdl <- isis_mdl(model_text = model_text, period = period, silent = TRUE)
  
  observed_data <- tribble(
    ~var, ~period,
    "y1", "2020"
  )
  
  # z1 is exogenous, so it should fail if provided as final destination
  final_destinations_bad <- tribble(
    ~var, ~period,
    "z1", "2021"
  )
  
  expect_error(
    mdl$find_deps(final_destinations = final_destinations_bad, observed_data = observed_data),
    "The following final destination variables are not endogenous model variables:\nz1.",
    fixed = TRUE
  )
  
  # z2 as final source should also fail
  final_sources_bad <- tribble(
    ~var, ~period,
    "z2", "2021"
  )
  final_destinations <- tribble(
    ~var, ~period,
    "y3", "2021"
  )
  
  expect_error(
    mdl$find_deps(final_destinations = final_destinations, 
                  final_sources = final_sources_bad, 
                  observed_data = observed_data),
    "The following final source variables are not endogenous model variables:\nz2.",
    fixed = TRUE
  )
})

test_that("find_deps overlap checks", {
  mdl <- isis_mdl(model_text = model_text, period = period, silent = TRUE)
  
  final_destinations <- tribble(
    ~var, ~period,
    "y3", "2021"
  )
  
  observed_data <- tribble(
    ~var, ~period,
    "y1", "2020",
    "y2", "2021"
  )
  
  solvable <- tribble(
    ~var, ~period,
    "y2", "2021"
  )
  
  # 1. solvable vs observed
  expect_error(
    mdl$find_deps(final_destinations = final_destinations, 
                  solvable = solvable, 
                  observed_data = observed_data),
    "The following solvable vertices are actually observed:\ny2[2021]",
    fixed = TRUE
  )
  
  # 2. solvable vs final sources
  final_sources <- tribble(
    ~var, ~period,
    "y2", "2021"
  )
  # Remove y2 from observed to only trigger the solvable vs final_sources check
  observed_data_clean <- tribble(
    ~var, ~period,
    "y1", "2020"
  )
  
  expect_error(
    mdl$find_deps(final_destinations = final_destinations, 
                  solvable = solvable, 
                  final_sources = final_sources,
                  observed_data = observed_data_clean),
    "The following solvable vertices are also final sources:\ny2[2021]",
    fixed = TRUE
  )
  
  # 3. final destinations vs solvable
  final_destinations_solvable <- tribble(
    ~var, ~period,
    "y3", "2021"
  )
  solvable_dest <- tribble(
    ~var, ~period,
    "y3", "2021"
  )
  
  expect_error(
    mdl$find_deps(final_destinations = final_destinations_solvable, 
                  solvable = solvable_dest, 
                  observed_data = observed_data_clean),
    "The following final destination vertices are also solvable:\ny3[2021]",
    fixed = TRUE
  )
})
