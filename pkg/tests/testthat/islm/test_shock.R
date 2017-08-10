library(utils)
library(isismdl)
library(testthat)

context("shocks for the  ISLM model")

capture_output(mdl <- read_mdl("islm_model_solved.rds"))

print(mdl$get_data())
g <- regts(c(10, 20), period = "2015Q2/2015Q3")
ms <-  regts(-10, period = "2015Q2")
shock <- cbind(g, ms)
print(shock)

mdl$set_data(shock, fun = `+`, upd_mode = "updval")
print(mdl$get_data())

mdl$set_ca(regts(-5, period = "2015Q2/2015Q4"), names = "c", fun = `+`)
print(mdl$get_ca())

mdl$solve()
