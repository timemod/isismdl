library(isismdl)

mdl <- isis_mdl("test1.mdl", period = "2018/2020")

mdl$set_values(1, names = "x")

mdl$solve(options = list(maxiter = 0))
print(mdl$get_solve_status())

mdl$solve()

print(mdl$get_endo_names(type = "feedback"))
