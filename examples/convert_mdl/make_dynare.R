library(isismdl)
library(dynmdl)

convert_mdl_file("../islm/islm.mdl", "islm.mod",
                 conversion_options = list(make_dynare = TRUE));
dynmdl <- dyn_mdl("islm.mod")

convert_mdl_file("pr_2013.mdl", "pr_2013.mod",
                 conversion_options = list(make_dynare = TRUE))

# use_dll = FALSE is now working yet, the R code is not correct.
dynmdl <- dyn_mdl("pr_2013.mod", use_dll = TRUE)
