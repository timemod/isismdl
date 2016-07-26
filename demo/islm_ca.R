library(regts)
library(macromod)

islm_model <- read_mdl("demo/islm.mif")
islm_model
islm_model$get_variable_names()
islm_model$set_period("2010Q3/2011Q4")
data <- regts(matrix(NA, ncol = 6), start = "2010Q2", end =  "2011Q4",
              names = c("r", "y", "yd", "x", "g", "ms"))
data[, 'r'] <- 3.35
data[, 'y'] <- 980
data[, 'yd'] <- 790
data[, 'g'] <- 210
data[, 'ms'] <- 200

islm_model$set_data(data)

ca_data <- regts(matrix(2, ncol = 2), start = "2010Q2", end =  "2010Q4",
              names = c("c", "x"))
islm_model$set_ca(ca_data)
result <- islm_model$solve("2010Q3/2011Q4")
print(result)
print(islm_model$get_data())
print(islm_model$get_ca_names())
print(islm_model$get_ca())
print(islm_model$get_ca(period = "2010Q3/2010Q4"))
print(islm_model$get_ca("c"))
print(islm_model$get_ca("x"))
