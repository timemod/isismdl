library(utils)

capture.output(islm_model <- islm_mdl("2015Q2/2016Q3"))
islm_model$saveRDS("islm_model.rds")

capture.output(islm_model <- islm_mdl_S4("2015Q2/2016Q3"))
saveRDS(islm_model, "islm_model_S4.rds")
