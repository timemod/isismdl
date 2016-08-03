input_file <- "input/input_mws.RData"
mif_file <- "mdl/islm.mif"

islm_model <- MacroModel$new(mif_file)

p1 <- start_period(islm_input) + islm_model$maxlag
p2 <- end_period(islm_input)
islm_model$set_period(regperiod_range(p1, p2))
islm_model$set_data(islm_input)

input_mws <- islm_model$get_mws()

save(file = input_file, input_mws)
