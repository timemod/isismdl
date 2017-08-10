library(utils)
library(isismdl)
library(testthat)

context("equation debugging for the ISLM model")

capture_output(mdl <- read_mdl("islm_model.rds"))

mdl$set_debug_eqn(TRUE)
print(mdl$get_debug_eqn())

mdl2 <- mdl$copy()
mdl2$set_values(0, names = "ms", period = "2015Q2/2015Q3")
print(mdl2$get_data())
mdl2$solve()
print(mdl2$get_data())

mdl3 <- mdl$copy()
mdl3$set_values(0, names = "ms", period = "2015Q2/2015Q3")
print(mdl3$get_data())
mdl3$run_eqn()
print(mdl3$get_data())
