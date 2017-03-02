library(isismdl)

rds_file <- "islm_basis.rds"
if (!file.exists(rds_file)) {	
    stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl_S4(rds_file)

mdl <- set_values(mdl, NA, names = "y")

mdl <- fill_mdl_data(mdl)
print(mdl_data(mdl))
print(ca(mdl))
