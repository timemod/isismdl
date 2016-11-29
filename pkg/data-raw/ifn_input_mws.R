library(data.table)
library(regts)
library(isismdl)

# read input data
input_file <- "ifn_input.csv"
input_df <- fread(input = input_file, data.table = FALSE)
input_data <- as.regts(input_df, time_column = 1)
colnames(input_data) <- tolower(colnames(input_data))

copy_example_mdl("ifn")
ret <- compile_mdl("ifn.mdl")

ifn <- IsisMdl$new("ifn.mif")
ifn$set_period("2/100")
ifn$set_data(input_data)
ifn$set_ftrelax(0.5, names = "lambda")
ifn$set_solve_options(xmaxiter = 1500, ratreport = "iter",
                      report = "minimal", ratreport_rep =c(10, 50))

# get the input mws and copy to data
ifn_input_mws <- ifn$get_mws()
devtools:::use_data(ifn_input_mws, overwrite = TRUE)

# solve the model for testing
ifn$solve()

# clean up stuff
unlink("ifn.mdl")
unlink("ifn.mif")
unlink("ifn.mrf")
