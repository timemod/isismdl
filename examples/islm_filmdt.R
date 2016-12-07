library(regts)
library(isismdl)

mdl <- read_mdl("islm.mif")
load("basis_mws.RData")
mdl$set_mws(basis_mws)
mdl$set_data(regts(NA, period = mdl$get_period()), names = "y")
print(mdl$get_data())
mdl$fill_mdl_data()
print(mdl$get_data())

