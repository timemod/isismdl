library(isismdl)

rds_file <- "ifn_basis.rds"
if (!file.exists(rds_file)) {
    stop("No rds file with model present. Run job ifn_basis.R first")
}
ifn <- read_mdl(rds_file)

ifn$set_solve_options(report = "minimal", ratreport = "fullrep",
                      ratreport_rep = 10, ratfullreport_rep = NA)
ifn$solve()
#print(ifn$get_solve_options()$ratreport_rep)
