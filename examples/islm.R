library(regts)
library(isismdl)
islm_model <- read_mdl("islm.mif")
print(islm_model$get_var_names())
islm_model$set_mws(islm_input_mws)

#islm_model$solve()
d1 <-islm_model$get_data()
islm_model$mdlpas()
d2 <- islm_model$get_data()

print(d1)
print(d2)
