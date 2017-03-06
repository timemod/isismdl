library(isismdl)
library(data.table)

input_file <- "ifn_input.csv"
input_df <- fread(input = input_file, data.table = FALSE)
input_data <- as.regts(input_df, time_column = 1)
colnames(input_data) <- tolower(colnames(input_data))

ifn <- isis_mdl("ifn.mdl", period = "2/100", data = input_data)
ifn$set_ftrelax(0.5, names = "lambda")
ifn$set_solve_options(xmaxiter = 1500, ratreport = "iter",
                      report = "minimal", ratreport_rep =c(10, 50))

ifn$write_mdl("ifn_basis.rds")
