library(regts)
library(isismdl)

rds_file <- "islm_basis.ismdl"
if (!file.exists(rds_file)) {
  stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl(rds_file)

mdl$set_data(regts(NA, period = mdl$get_period()), names = "y")
print(mdl$get_data())
mdl$fill_mdl_data()
print(mdl$get_data())

