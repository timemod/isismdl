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

fix <- regts(matrix(NA, ncol = 3), start = "2010Q3", end =  "2011Q1",
              names = c("i", "c", "r"))
fix[, 'c'] <- 600
fix['2010Q3', 'i'] <- 200
fix['2010Q3', 'r'] <- 800
islm_model$set_fix(fix)
print(islm_model$get_fix())

result <- islm_model$solve("2010Q3/2011Q4")
print(result)
print(islm_model$get_data())

