library(regts)
library(isismdl)
islm_model <- IsisMdl$new("demo/islm.mif")
islm_model$set_period(islm_input_mws$model_period)
islm_model$set_solve_options(mode = "ratex")
print(islm_model$get_solve_options())
islm_model$set_data(islm_input_mws$data)
islm_model$solve()

