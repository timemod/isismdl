library(regts)
library(macromod)
library(microbenchmark)

islm_model <- MacroModel$new("demo/islm.mif")
islm_model
islm_model$get_variable_names()
islm_model$set_period("2010Q3/2011Q4")
islm_model$set_data(islm_input)

y <-regts(c(1000, 1001), start = "2010Q3", end =  "2010Q4")
islm_model$set_fit(y)

rms_values <- list(c = 5, i = 21, md = 2, t = 2)
islm_model$set_rms(rms_values)
result <- islm_model$solve("2010Q3/2011Q4")
print(result)
print(islm_model$get_data())
print(islm_model$get_fit())
