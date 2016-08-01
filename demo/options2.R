library(regts)
library(macromod)

islm_model <- MacroModel$new("demo/islm.mif")
islm_model$set_period("2010Q3/2011Q4")
islm_model$get_solve_options()
islm_model$set_solve_options(list(mode = "static"))
islm_model$get_solve_options()

