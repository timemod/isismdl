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

test_that("find_deps model property checks", {
  period <- as.period_range("2020/2022")

  # 1. Model with leads
  model_leads <- "ident y = y(1) + x;"
  mdl_leads <- isis_mdl(model_text = model_leads, period = period, silent = TRUE)
  expect_error(
    mdl_leads$find_deps(final_destinations = tribble(~var, ~period, "y", "2021"),
                        observed_data = tribble(~var, ~period, "x", "2021")),
    "find_deps does not support models with leads.",
    fixed = TRUE
  )

  # 2. Simultaneous model
  # This should trigger the recursive check first
  # but without recursive check it would trigger the feedback check
  model_simul <- "
    ident y1 = y2 + x;
    ident y2 = y1 + z;
  "
  mdl_simul <- isis_mdl(model_text = model_simul, period = period, silent = TRUE)
  expect_error(
    mdl_simul$find_deps(final_destinations = tribble(~var, ~period, "y1", "2021"),
                        observed_data = tribble(~var, ~period, "x", "2021", "z", "2021")),
    "find_deps only supports recursive models.",
    fixed = TRUE
  )
})

test_that("find_deps empty observed data", {
  period <- as.period_range("2020/2022")
  model_text <- "ident y = 0.8 * y(-1) + x;"
  mdl <- isis_mdl(model_text = model_text, period = period, silent = TRUE)

  # 1. Empty observed data should not crash
  # It should default to model data start and successfully find deps
  final_dest <- tribble(~var, ~period, "y", "2021")

  g <- mdl$find_deps(final_destinations = final_dest, 
                     observed_data = tribble(~var, ~period))
  expect_s3_class(g, "igraph")
  expect_true("y[2021]" %in% V(g)$name)
  expect_true("y[2020]" %in% V(g)$name)

})

test_that("find_deps vertex colors", {
  period <- as.period_range("2020/2022")
  model_text <- "
    ident y1 = y1(-1) + x1;
    ident y2 = y1 + x2;
    ident y3 = y2 + y1(-1);
  "
  mdl <- isis_mdl(model_text = model_text, period = period, silent = TRUE)
  
  observed <- tribble(
    ~var, ~period,
    "y1", "2020",
    "x1", "2021",
    "x2", "2021"
  )
  solvable <- tribble(~var, ~period, "y1", "2021")
  final_dest <- tribble(~var, ~period, "y3", "2021")
  
  expect_warning(
    g <- mdl$find_deps(final_destinations = final_dest, 
                       solvable = solvable, 
                       observed_data = observed),
    "The following solvables are calculable, no need to solve them:"
  )
  
  # NOTE: y1[2020] and x2[2021] are pruned because they are not reachable FROM y1[2021]
  
  expect_equal(V(g)["y3[2021]"]$color, "cyan")
  expect_equal(V(g)["y2[2021]"]$color, "green")
  expect_equal(V(g)["y1[2021]"]$color, "pink")
  
  final_src <- tribble(~var, ~period, "y1", "2021")
  g_src <- mdl$find_deps(final_destinations = final_dest, 
                         final_sources = final_src,
                         observed_data = observed)
  
  expect_equal(V(g_src)["y1[2021]"]$color, "yellow")
  expect_equal(V(g_src)["y1[2020]"]$color, "lightgrey")
  expect_equal(V(g_src)["x2[2021]"]$color, "lightgrey")
  
  g_err <- mdl$find_deps(final_destinations = final_dest,
                         observed_data = observed[1, ])
  expect_equal(V(g_err)["y3[2021]"]$color, "purple")
  expect_equal(V(g_err)["y2[2021]"]$color, "red")
})

test_that("find_deps vertex attributes internal metadata", {
  period <- as.period_range("2020/2022")
  model_text <- "
    ident y1 = x1;
    ident y2 = y1 + x2;
  "
  mdl <- isis_mdl(model_text = model_text, period = period, silent = TRUE)

  solvable <- tribble(~var, ~period, "y1", "2021")
  final_dest <- tribble(~var, ~period, "y2", "2021")
  observed <- tribble(~var, ~period, "x1", "2021", "x2", "2021")

  expect_warning(
    g <- mdl$find_deps(final_destinations = final_dest,
                       solvable = solvable,
                       observed_data = observed),
    "The following solvables are calculable, no need to solve them:"
  )

  # Verify attributes are set correctly during add_vertex and recursion
  expect_true(V(g)["y2[2021]"]$is_final_dest)
  expect_false(V(g)["y1[2021]"]$is_final_dest)

  expect_true(V(g)["y1[2021]"]$solvable)
  expect_false(V(g)["y2[2021]"]$solvable)

  expect_true(V(g)["y1[2021]"]$ok) # solvable/calculable
  expect_true(V(g)["y2[2021]"]$ok)

  final_src <- tribble(~var, ~period, "y1", "2021")
  g2 <- mdl$find_deps(final_destinations = final_dest,
                      final_sources = final_src,
                      observed_data = observed)

  expect_true(V(g2)["y1[2021]"]$is_final_src)
  expect_false(V(g2)["y2[2021]"]$is_final_src)
})

test_that("find_deps warning for calculable solvables", {
  period <- as.period_range("2020/2022")
  model_text <- "
    ident y1 = x1;
    ident y2 = y1 + x2;
  "
  mdl <- isis_mdl(model_text = model_text, period = period, silent = TRUE)

  # x1 and x2 are observed in 2021.
  # So y1 is "calculable" in 2021 (y1 = x1).
  # If we mark y1 as solvable, it should trigger a warning.
  observed <- tribble(~var, ~period, "x1", "2021", "x2", "2021")
  solvable <- tribble(~var, ~period, "y1", "2021")
  final_dest <- tribble(~var, ~period, "y2", "2021")

  expect_warning(
    mdl$find_deps(final_destinations = final_dest,
                  solvable = solvable,
                  observed_data = observed),
    "The following solvables are calculable, no need to solve them:"
  )
})

test_that("find_deps message for removed solvables", {
  period <- as.period_range("2020/2022")
  model_text <- "
    ident y1 = 1;
    ident y2 = y1 + x2;
    ident y3 = y2;
  "
  mdl <- isis_mdl(model_text = model_text, period = period, silent = TRUE)

  # x2 is missing, so y2 is not OK.
  # Since y2 is not OK and not a final destination, its incoming edges (y1 -> y2)
  # will be deleted.
  # Now y1 cannot reach the final destination y3.
  # If y1 is marked as solvable, it should be removed with a message.
  
  final_dest <- tribble(~var, ~period, "y3", "2021")
  solvable <- tribble(~var, ~period, "y1", "2021")
  
  # We expect a message about y1 being removed.
  # Note: y3 will still be in the graph but marked as not OK (purple color).
  expect_message(
    mdl$find_deps(final_destinations = final_dest,
                  solvable = solvable,
                  observed_data = tribble(~var, ~period)),
    "The following solvable variables have been removed because there is not a calculable path to observations:"
  )
})
