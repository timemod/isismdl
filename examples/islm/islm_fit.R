library(regts)
library(isismdl)

rm(list = ls())

rds_file <- "islm_basis.ismdl"
if (!file.exists(rds_file)) {
    stop("No rds file with model present. Run job islm_basis.R first")
}
mdl <- read_mdl(rds_file)
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
mdl$set_fit_options(zealous = TRUE)
mdl$solve()
print(mdl$get_data())
print(mdl$get_ca())

print(mdl$get_fit_options())
