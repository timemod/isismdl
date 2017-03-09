library(regts)
library(isismdl)

rds_file <- "islm_basis.rds"
if (!file.exists(rds_file)) {	
    stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl(rds_file)


mdl$set_ca_values(4500, names = "c", period = "2015Q1/2015Q2")
mdl$set_ca_values(-4500, names = "t", period = "2015Q1/2015Q2")
mdl$set_fix_values(333, names = "c", period = "2015Q1/2015Q3")
print(mdl$get_ca())
print(mdl$get_fix())
mdl$run_eqn(c("c", "t"))
print(mdl$get_data())

