library(isismdl)
library(data.table)

input_file <- "ifn_input.csv"
input_df <- fread(input = input_file, data.table = FALSE)
input_data <- as.regts(input_df, time_column = 1)
colnames(input_data) <- tolower(colnames(input_data))

ifn <- isis_mdl("ifn.mdl", data = input_data)
ifn$set_ftrelax(0.5, names = "lambda")
ifn$set_solve_options(xmaxiter = 1500, report = "minimal",
                      ratreport = "fullrep", ratreport_rep = 10)

ifn$write_mdl("ifn_basis.rds")
