library(regts)
library(macromod);

model <- read_mdl("example/mini.mif")
model
model$get_variable_names()
model$set_period("2010Q2/2011Q4")
data <- regts(matrix(1, ncol = 2), start = "2010Q2", end =  "2011Q4",
              names = c("X", "Y"))
model$set_data(data)
#model$run_equations("2010Q2")
model$solve("2010Q2/2011Q4")
print(model$get_data())
