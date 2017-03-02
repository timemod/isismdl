library(isismdl)
library(BB)

# solve the endogenous leads of the IFN model with dfsane

rds_file <- "ifn_basis.rds"
if (!file.exists(rds_file)) {	
    stop("No rds file with model present. Run job ifn_basis.R first")
}
ifn <- read_mdl(rds_file)

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

ret <- dfsane(start, fun, method = 2, control = list(tol = 1e-7))
print (ret$convergence)

print(sum(fun(as.numeric(get_endoleads()))))

print(ifn$get_solve_options()$report)
ifn$solve(options = list(mode = "ratex"))



