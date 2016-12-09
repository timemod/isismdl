library(regts)
library(isismdl)

mif_file <- "islm.mif"
mws_file <- "basis_mws.RData"

if (!file.exists(mif_file)) {	
    stop("No mif and mws file available. Run job islm_basis.R first")
}

mdl <- read_mdl(mif_file)
load(mws_file)
mdl$set_mws(basis_mws)

mdl$set_data(regts(NA, period = mdl$get_period()), names = "y")
print(mdl$get_data())
mdl$fill_mdl_data()
print(mdl$get_data())

