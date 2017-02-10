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

y <- regts(985, start = "2015Q2")
r <- regts(c(3.5, 3.6), start = "2015Q2")
fit <- cbind(y, r)
mdl$set_fit(fit)
cat("fit targets:\n")
print(mdl$get_fit())
rms_values <- c(c = 5.0, i = 21, md = 2, t = 2)
print(rms_values)
print(names(rms_values))
mdl$set_rms(rms_values)
mdl$set_solve_options(report = "period")
mdl$set_fit_options(report = "mininal")
mdl$solve()
print(mdl$get_data())
View(mdl$get_data())
print(mdl$get_ca())
