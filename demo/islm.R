library(regts)
library(macromod)
islm_model <- MacroModel$new("demo/islm.mif")
print(islm_model$get_variable_names())
islm_model$set_period("2015Q2/2016Q3")
islm_model$set_data(islm_input)$solve()
islm_model$get_data()
system.time(
islm_model$solve(options = list(mode = "dynamic"))
)

