library(utils)
library(isismdl)
library(testthat)

rm(list = ls())

update_expected <- FALSE

compile_mdl <- function(model_text, flags = character(0)) {
  mdl <- isis_mdl(model_text = model_text, silent = TRUE,
                  parse_options = list(flags = flags))
  return(mdl$get_text())
}

model_text <- glue::glue("
     x = 1;
     #if a1
       a1 = 1;
       #if a1_b1
          a1_b1 = 1;
       #elseif a1_b2
          a1_b2 = 1;
       #endif
     #elseif a2
       a2 = 1;
       #if a2_b1
          a2_b1 = 1;
       #elseif a2_b2
          a2_b2 = 1;
          #if a2_b2_c1
              a2_b2_c1 = 1;
          #elseif a2_b2_c2
              a2_b2_c2 = 1;
          #else
              a2_b2_c3 = 1;
          #endif
       #else
          a2_b3 = 1;
       #endif
     #elseif a3
       a3 = 1;
       #if a3_b1
          a3_b1 = 1;
       #else
          a3_b2 = 1;
          #if a3_b2_c1
              a3_b2_c1 = 1;
          #elseif a3_b2_c2
              a3_b2_c2 = 1;
          #else
              a3_b2_c3 = 1;
          #endif
      #endif
     #else
       a4 = 1;
     #endif
     y = 2;
    ")

test_that("#if statements are parsed correctly", {

  flags <- c(
    "",
    "a1",
    "a2__a2_b2",
    "a3__a2_b2__a2_b2_c1",
    "a3",
    "a3__a3_b1",
    "a3__a3_b2__a3_b2_c2"
  )

  for (flag in flags) {
    expect_known_output(
     cat(compile_mdl(model_text, flags = strsplit(flag, "__")[[1]])),
     sprintf("expected_output/conditional2_%s.txt", flag),
     update = update_expected
   )
  }
})
