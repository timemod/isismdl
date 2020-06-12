library(isismdl)

mdl <- read_mdl("example_v120a.ismdl")
#mdl$set_fit_options(zealous = FALSE, cvgrel = 1)
print(mdl$get_fit_options())
mdl$solve(period = "2020Q2")
