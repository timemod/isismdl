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

mdl$set_values(3, pattern = "^c$", period = "2015Q2/2016Q1")
print(mdl$get_data())

mdl$set_ca_values(c(8,9), names = "c", period = "2015Q2/2015Q3")
print(mdl$get_ca())

mdl$change_data(fun = function(x) {x  + 100}, names = "i")
print(mdl$get_data())

