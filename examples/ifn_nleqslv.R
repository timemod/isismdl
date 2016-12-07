library(isismdl)
library(nleqslv)

# solve the endogenous leads of the IFN model with nleqslv

ret <- compile_mdl("ifn.mdl")
ifn <- read_mdl("ifn.mif")
ifn$set_mws(ifn_input_mws)

get_endoleads <- function() {
    return (ifn$get_data(names = ifn$get_var_names(vtype = "all_endolead"),
                         period = ifn$get_period()))
}

fun <- function(x) {
    vnames <- ifn$get_var_names(vtype = "all_endolead")
    endoleads <- regts(matrix(x, ncol = length(vnames)), names = vnames,
                               start = start_period(ifn$get_period()))
    ifn$set_data(endoleads)
    ifn$solve(options = list(mode = "dynamic", report = "none"))
    endoleads2 <- get_endoleads()
    return (as.numeric(endoleads) - as.numeric(endoleads2))
}

start <-as.numeric(get_endoleads())

print(sum(fun(start)))

ret <- nleqslv(start, fun, control = list(ftol = 1e-8))
print(ret)

print(sum(fun(as.numeric(get_endoleads()))))

ifn$solve(options = list(mode = "ratex"))



