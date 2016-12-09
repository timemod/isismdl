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

# create fix targets
c_fix <- regts(600, period = mdl$get_period())
i_fix <- regts(200, period = "2015Q2")
fix_data <- cbind(c = c_fix, i = i_fix)
mdl$set_fix(fix_data)
cat("fit targets:\n")
print(mdl$get_fix())

mdl$solve()
print(mdl$get_data())
