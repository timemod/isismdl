library(regts)
library(macromod)

islm_model <- read_mdl("example/islm.mif")
islm_model
islm_model$get_variable_names()
islm_model$set_period("2010Q3/2011Q4")
data <- regts(matrix(NA, ncol = 5), start = "2010Q2", end =  "2011Q4",
              names = c("R", "Y", "YD", "G", "MS"))
data[, 'Y'] <- 980
data[, 'YD'] <- 790
data[, 'G'] <- 210
data[, 'MS'] <- 200
islm_model$set_data(data)
result <- islm_model$solve("2010Q3/2011Q4")
print(result)
#print(islm_model$get_data())
