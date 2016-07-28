library(regts)
library(macromod)

islm_model <- MacroModel$new("demo/islm.mif")
print(islm_model$get_variable_names())
islm_model$set_period("2010Q3/2011Q4")
islm_model$set_data(islm_input)$solve()
print(islm_model$get_data())
islm_model$set_data(islm_input)$solve(options = list(mode = "reschk"))
print(islm_model$get_data())

