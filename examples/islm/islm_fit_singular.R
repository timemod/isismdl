library(regts)
library(isismdl)

rds_file <- "islm_basis.ismdl"
if (!file.exists(rds_file)) {
    stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl(rds_file)

y <- regts(985, start = "2015Q2")
c <- regts(c(600, 1200), start = "2015Q2")
yd <- regts(c(950, 940), start = "2015Q2")
fit <- cbind(y, c, yd)
mdl$set_fit(fit)
cat("fit targets:\n")
print(mdl$get_fit())
rms_values <- c(c = 5.0, i = 2, md = 3)
print(rms_values)
print(names(rms_values))
mdl$set_rms(rms_values)
mdl$set_solve_options(report = "period")
mdl$set_fit_options(zealous = TRUE, svdtest_tol = 1e-4, dbgopt = "prijac")
mdl$solve()
print(mdl$get_data())
print(mdl$get_ca())

print(mdl$get_fit_options())
