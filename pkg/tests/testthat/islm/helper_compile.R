library(utils)

capture.output(islm_model <- islm_mdl("2015Q2/2016Q3"))
islm_model$saveRDS("islm_model.rds")
