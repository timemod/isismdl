create_data <- function(names, period) {
    nvar <- length(names)
    nper <- length_range(period)
    return(regts(matrix(NA, ncol = nvar, nrow = nper),
                  period = period, names = names))
}

create_ca <- function(names, period) {
    nvar <- length(names)
    nper <- length_range(period)
    return(regts(matrix(0, ncol = nvar, nrow = nper), period = period,
                  names = names))
}

# Convert input timeseries. Uses the intersection with names and period.
convert_data <- function(data, names, period) {
    if (!is.ts(data)) {
        stop("data is not a timeseries object")
    }
    if (!is.matrix(data)) {
        stop("Non-matrix type in input timeseries are not yet supported")
    }
    if (is.null(colnames(data))) {
        stop("Input timeseries without column names are not yet supported")
    }

    # make sure that data is a matrix of numeric values
    if (is.integer(data) || !is.numeric(data)) {
        data <- apply(data, MARGIN = c(1,2), FUN = as.numeric)
    }

    names <- intersect(names, colnames(data))
    p <- regrange_intersect(period, get_regperiod_range(data))
    return(data[p, names, drop = FALSE])
}
