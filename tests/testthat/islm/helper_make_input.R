input_file <- "input/input.RData"
model_period <- regperiod_range("2015Q2", "2016Q3")

# create input timeseries. input contains an additional timeseries x that
# is no model timeseries. This is done on purpose, set_mdl_data should ignore
# this extra variable.
input <- regts(matrix(NA, ncol = 6), start = "2015Q1", end = "2016Q3",
               names = c("r", "y", "x", "yd", "g", "ms"))

input[, 'r'] <- 3.35
input[, 'y'] <- 980
input[, 'yd'] <- 789
input[model_period, 'g']  <- c(210, 213, 216, 219, 222, 225)
input[model_period, 'ms'] <- c(200, 203, 206, 209, 212, 215)

save(file = input_file, list = c("model_period", "input"))
