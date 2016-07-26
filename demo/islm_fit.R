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

fit <- regts(matrix(NA, ncol = 2), start = "2010Q3", end =  "2010Q4",
             names = c("y", "c"))
fit[ , 'y'] <- 985
fit[ , 'r'] <- 3.5
islm_model$set_fit(fit)

rms_values <- list(c = 5, i = 21, md = 2, t = 2)
islm_model$set_rms(rms_values)
result <- islm_model$solve("2010Q3/2011Q4")
print(result)
print(islm_model$get_data())
print(islm_model$get_fit())
