library(isismdl)

rds_file <- "islm_basis.rds"
if (!file.exists(rds_file)) {	
    stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl_S4(rds_file)

c_fix <- regts(600, period = mdl@period)
i_fix <- regts(200, period = "2015Q2")
fix_data <- cbind(c = c_fix, i = i_fix)

mdl <- set_fix(mdl, data =  fix_data)
mdl <- solve_mdl(mdl)
print(mdl_data(mdl))
print(ca(mdl))
