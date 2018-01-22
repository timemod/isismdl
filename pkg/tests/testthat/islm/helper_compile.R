library(utils)

capture.output(islm_model <- islm_mdl("2015Q2/2016Q3"))
islm_model$write_mdl("islm_model.ismdl")

islm_model$solve(options = list(report = "none"))
islm_model$write_mdl("islm_model_solved.ismdl")
