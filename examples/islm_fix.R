library(regts)
library(isismdl)

mdl <- read_mdl("islm.mif")

load("basis_mws.RData")
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
