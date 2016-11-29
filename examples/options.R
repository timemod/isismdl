library(regts)
library(isismdl)

islm_model <- IsisMdl$new("demo/islm.mif")
print(islm_model$get_variable_names())
islm_model$set_period("2010Q3/2011Q4")
islm_model$set_solve_options(mode = "dynamic", 2)

