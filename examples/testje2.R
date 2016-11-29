library(regts)
library(isismdl)
islm_model <- IsisMdl$new("demo/islm.mif")
islm_model$set_mws(islm_input_mws)

system.time(
    mws <- islm_model$get_mws()
)
system.time(
    islm_model$set_mws(mws)
)
