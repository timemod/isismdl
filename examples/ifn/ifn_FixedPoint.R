library(isismdl)
library(FixedPoint)

# solve the endogenous leads of the IFN model with nleqslv

rds_file <- "ifn_basis.rds"
if (!file.exists(rds_file)) {
  stop("No rds file with model present. Run job ifn_basis.R first")
}
ifn <- read_mdl(rds_file)


get_endoleads <- function() {
  return (ifn$get_data(names = ifn$get_endo_names(type = "endolead"),
                       period = ifn$get_period()))
}

fun <- function(x) {
  vnames <- ifn$get_endo_names(type = "endolead")
  endoleads <- regts(matrix(x, ncol = length(vnames)), names = vnames,
                     start = start_period(ifn$get_period()))
  ifn$set_data(endoleads)
  ifn$solve(options = list(mode = "dynamic", report = "none"))
  endoleads2 <- get_endoleads()
  return(as.numeric(endoleads2))
}

start <-as.numeric(get_endoleads())

print(sum(fun(start)))

ret <- FixedPoint(fun, start,  ConvergenceMetricThreshold = 1e-8, Method = "VEA")
print(ret)

ifn$solve(options = list(mode = "ratex"))



