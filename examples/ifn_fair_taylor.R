library(isismdl)

ret <- compile_mdl("ifn.mdl")
ifn <- read_mdl("ifn.mif")
ifn$set_mws(ifn_input_mws)
ifn$set_solve_options(report = "minimal", ratreport = "fullrep",
                      ratreport_rep = c(10, 50))
ifn$solve()
print(ifn$get_solve_options()$ratreport_rep)
