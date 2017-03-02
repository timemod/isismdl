library(isismdl)

rds_file <- "islm_basis.rds"
if (!file.exists(rds_file)) {	
    stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl_S4(rds_file)

y <- regts(985, start = "2015Q2")
r <- regts(c(3.5, 3.6), start = "2015Q2")
fit <- cbind(y, r)
mdl <- set_fit(mdl, fit)
cat("fit targets:\n")
print(fit_targets(mdl))
rms_values <- c(c = 5.0, i = 21, md = 2, t = 2)
mdl <- set_rms(mdl, rms_values)
mdl <- solve_mdl(mdl)
print(mdl_data(mdl))
print(ca(mdl))
