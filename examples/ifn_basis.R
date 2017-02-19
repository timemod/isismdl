library(isismdl)
library(data.table)

ifn <- compile_mdl("ifn.mdl")

input_file <- "ifn_input.csv"
input_df <- fread(input = input_file, data.table = FALSE)
input_data <- as.regts(input_df, time_column = 1)
colnames(input_data) <- tolower(colnames(input_data))

copy_example_mdl("ifn")
ifn <- compile_mdl("ifn.mdl")
ifn$set_period("2/100")
ifn$set_data(input_data)
ifn$set_ftrelax(0.5, names = "lambda")
ifn$set_solve_options(xmaxiter = 1500, ratreport = "iter",
                      report = "minimal", ratreport_rep =c(10, 50))


ifn$saveRDS("ifn_basis.rds")
