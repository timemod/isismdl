library(regts)
library(macromod)
islm_model <- MacroModel$new("demo/islm.mif")
print(islm_model$get_variable_names())
islm_model$set_mws(islm_input_mws)
islm_model$set_solve_options(list(mode = "dynamic", dbgopt = "prifb"))
islm_model$solve(options = list(mode = "dynamic", dbgopt = "prijac"))

