library(utils)
library(isismdl)

capture.output(ifn_mdl <- ifn_mdl())

input_file <- "input/input.csv"
input_df <- read.csv(file = input_file)
input_data <- as.regts(input_df, time_column = 1)
colnames(input_data) <- tolower(colnames(input_data))

ifn_mdl$init_data(data = input_data)

ifn_mdl$set_ftrelax(0.5, names = "lambda")
ifn_mdl$set_solve_options(xmaxiter = 1500, ratreport = "iter",
                          report = "minimal", ratreport_rep = 10,
                          ratfullreport_rep = 50)

ifn_mdl$write_mdl("ifn_mdl.rds")

