library(regts)
library(macromod)

islm_model <- MacroModel$new("demo/islm.mif")
islm_model$set_mws(islm_input_mws)

y <-regts(c(1000, 1001), start = "2015Q2", end =  "2016Q3")
islm_model$set_fit(y)

rms_values <- list(c = 5, i = 21, md = 2, t = 2)
islm_model$set_rms(rms_values)
islm_model$solve("2015Q2/2016Q3")
print(islm_model$get_data())
print(islm_model$get_fit())
