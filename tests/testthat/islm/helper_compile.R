library(utils)
copy_example_mdl("islm", "mdl/islm.mdl")
output <- capture.output(ret <- compile_mdl("mdl/islm.mdl"))
