library(isismdl)

ret <- compile_mdl("demo/ifn.mdl")
ifn <- IsisMdl$new("demo/ifn.mif")
ifn$set_mws(ifn_input_mws)
ifn$set_solve_options(report = "minimal", ratreport = "fullrep")
ifn$solve()
