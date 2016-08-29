library(regts)
library(isismdl)
islm_model <- IsisMdl$new("demo/islm.mif")
print(islm_model$get_variable_names())
print(islm_model$get_param_names())
islm_model$set_mws(islm_input_mws)
islm_model$solve();
islm_model$set_param(list(x = 2, c0 = 100, z = 120))
islm_model$get_param()
islm_model$get_param(names = "c0")
islm_model$get_param(names = "aap")
islm_model$solve()

