library(regts)
library(isismdl)

rds_file <- "islm_basis.rds"
if (!file.exists(rds_file)) {
    stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl(rds_file)
mdl$order(orfnam = "aap/islm.orf")
#mdl$order(orfnam = "islm.orf")
