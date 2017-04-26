# Update the data and ca slots of the IsisMdl object after
# some action has taken place in Fortran memory (solve, fillmdt, etc.)
# INPUT:
#   mdl    the IsisMdl object
#   period the model period IN FORTRAN MEMORY (this can be
#          different from mdl@period!
# OUTPUT:
#   mdl   a new IsisMdl object
update_data_ca <- function(mdl, period) {

    data <- get_data(mdl, period = period)
    ca   <- get_ca(mdl,   period = period)

    mdl@data[period, ] <- data

    # update CAs only if the fix or fit procedure have been used.
    if (!(is.null(mdl@fix) && is.null(mdl@fit))) {
        mdl@ca[period,   ] <- ca
    }
    return(invisible(mdl))
}

# Returns the model data as present in Fortran memory
# for the specified period.
# INPUT:
#   mdl    the IsisMdl object
#   period the model period IN FORTRAN MEMORY (this can be
#          different from mdl@period!
# OUTPUT:  a regts with all model data
get_data <- function(mdl, period) {
    names <- mdl@names
    nper <- nperiod(period)
    data <- .Call("get_data_c", type = "data", model_index = mdl@control$index,
                  names = names, jtb = 1L, jte = nper)
    return(regts(data, period = period, names = names))
}

# Returns the model data as present in Fortran memory
# for the specified period.
# INPUT:
#   mdl    the IsisMdl object
#   period the model period IN FORTRAN MEMORY (this can be
#          different from mdl@period!
# OUTPUT:  a regts with all constant adjustments
get_ca <- function(mdl, period) {
    names <- mdl@ca_names
    if (length(names) == 0) {
        return(NULL)
    }
    nper  <- nperiod(period)
    data <- .Call("get_data_c", type = "ca", model_index = mdl@control$index,
                  names = names, jtb = 1L, jte = nper)
    return(regts(data, period = period, names = names))
}
