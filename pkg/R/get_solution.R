get_solution <- function(model_index, period) {

    names <- .Call(get_var_names_c, "all", model_index)
    ca_names <- .Call(get_var_names_c, "allfrml", model_index)

    js <- get_period_indices(period, period)

    data <- .Call("get_data_c", type = "data", model_index = model_index,
                  names = names, jtb = js$startp, jte = js$endp)
    ca <- .Call("get_data_c", type = "ca", model_index = model_index,
                  names = ca_names, jtb = js$startp, jte = js$endp)

    data <- regts(data, start = start_period(period), names = names)
    ca_data <- regts(ca, start = start_period(period), names = ca_names)
    return(list(data = data, ca = ca))
}
