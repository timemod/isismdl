library(regts)
library(isismdl)
islm_model<- IsisMdl$new("demo/islm.mif")
print(islm_model$get_var_names())
islm_model$set_mws(islm_input_mws)

feedback_vars = c("y", "r")

get_fbk <- function() {
    return (islm_model$get_data(names = feedback_vars,
                         period = islm_model$model_period))
}

fun <- function(x) {
    vnames <- feedback_vars
    fbk <- regts(matrix(x, ncol = length(vnames)), names = vnames,
                 start = start_period(islm_model$model_period))
    islm_model$set_data(fbk)
    islm_model$mdlpas()
    fbk2 <- get_fbk()
    return (as.numeric(fbk) - as.numeric(fbk2))
}

start <-as.numeric(get_fbk())
print(sum(fun(start)))

ret <- nleqslv(start, fun, control = list(ftol = 1e-4, trace = 1))
print(ret)

print(sum(fun(as.numeric(get_fbk()))))

