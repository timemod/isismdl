library(regts)
library(macromod)

islm_model <- MacroModel$new("demo/islm.mif")
islm_model$get_variable_names()
islm_model$set_mws(islm_input_mws)$fill_mdl_data()
print(islm_model$get_data())

