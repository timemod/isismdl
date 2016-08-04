library(regts)
library(macromod)

islm_model <- MacroModel$new("demo/islm.mif")
islm_model$set_mws(islm_input_mws)

fix <- regts(matrix(NA, ncol = 3), start = "2015Q2", end =  "2016Q3",
              names = c("i", "c", "r"))
fix[, 'c'] <- 600
fix['2010Q3', 'i'] <- 200
fix['2010Q3', 'r'] <- 800
islm_model$set_fix(fix)
print(islm_model$get_fix())

islm_model$solve()
print(islm_model$get_data())

