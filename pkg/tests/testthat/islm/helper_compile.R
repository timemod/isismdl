library(utils)
library(isismdl)

islm_model <- islm_mdl("2015Q2/2016Q3", silent = TRUE)
islm_model$write_mdl("islm_model.ismdl")

islm_model$solve(options = list(report = "none"))
islm_model$write_mdl("islm_model_solved.ismdl")
