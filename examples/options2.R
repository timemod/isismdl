library(regts)
library(isismdl)

islm_model <- IsisMdl$new("demo/islm.mif")
islm_model$set_period("2010Q3/2011Q4")
islm_model$get_solve_options()
islm_model$set_solve_options(list(mode = "static", xupdate = "lastval",
                                 dbgopt = "prifb"))
islm_model$get_solve_options()
