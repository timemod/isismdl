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
data[, 'x'] <- 2300
islm_model$set_data(data)
#result <- islm_model$solve("2010Q3/2011Q4")
result <- islm_model$solve()
print(result)
print(islm_model$get_data())

