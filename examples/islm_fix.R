library(regts)
library(isismdl)

rds_file <- "islm_basis.rds"
if (!file.exists(rds_file)) {	
    stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl(rds_file)

# create fix targets
c_fix <- regts(600, period = mdl$get_period())
i_fix <- regts(200, period = "2015Q2")
fix_data <- cbind(c = c_fix, i = i_fix)
mdl$set_fix(fix_data)
cat("fit targets:\n")
print(mdl$get_fix())

mdl$solve()
print(mdl$get_data())
