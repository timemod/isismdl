create_data <- function(names, period) {
    nvar <- length(names)
    nper <- length_range(period)
    return(regts(matrix(NA_real_, ncol = nvar, nrow = nper),
                  period = period, names = names))
}

create_ca <- function(names, period) {
    nvar <- length(names)
    nper <- length_range(period)
    return(regts(matrix(0, ncol = nvar, nrow = nper), period = period,
                  names = names))
}

# General function used to transfer timeseries to the model data,
# constant adjustments, fix or fit values.
# INPUT:
#      mdl               the model
#      type              type ("data", "ca", "fix" or "fit")
#      new_data          new timeseries data
#      new_names         names of the new timeseries data
#      new_names_missing TRUE if the names of the new timeseries has not been
#                        specicified
#  RETURN: updated model object
#
#' @importFrom stats is.ts
set_data_ <- function(mdl, type, new_data, new_names, new_names_missing) {

    if (!is.ts(new_data)) {
        stop("data is not a timeseries object")
    }

    if (NCOL(new_data) == 0) {
        # TODO: warning?
        return (invisible(NULL))
    }
    if (is.null(new_names)) {
        if (new_names_missing) {
            stop(paste("The input timeseries has no colnames.",
                       "In that case, argument names should be specified"))
        } else {
            stop("names is null")
        }
    }  else {
        if (length(new_names) < NCOL(new_data)) {
            stop("The length of arument names is less than the number of columns of data")
        }
    }

    # convert new_data to matrix if necessary
    if (!is.matrix(new_data)) {
        dim(new_data) <- c(length(new_data), 1)
        colnames(new_data) <- new_names
    }

    if (is.integer(new_data) || !is.numeric(new_data)) {
        # make sure that data is a matrix of numeric values
        new_data[, ] <- apply(new_data, MARGIN = c(1,2), FUN = as.numeric)
    }


    if (type == "data" || type == "fit") {
        data_names <- mdl@names
    } else {
        data_names <- mdl@ca_names
    }
    if (type == "data") {
        data_period <- mdl@data_period
    } else {
        data_period <- mdl@period
    }

    names <- intersect(new_names, data_names)
    p <- regrange_intersect(data_period, get_regperiod_range(new_data))

    new_data <-  new_data[p, names, drop = FALSE]

    if (type == "data") {
        mdl@data[p, names] <- new_data
    } else if (type == "ca") {
        mdl@ca[p, names] <- new_data
    } else if (type == "fix") {
        if (is.null(mdl@fix)) {
            mdl@fix <- new_data
        } else {
            mdl@fix[p, names] <- new_data
        }
    } else if (type == "fit") {
        if (is.null(mdl@fit)) {
            mdl@fit <- new_data
        } else {
            mdl@fit[p, names] <- new_data
        }
    }

    return(invisible(mdl))
}

# General function used to set model data, constant adjustments, fix values
# or fit values.
# INPUT:
#      mdl               the model
#      type              type ("data", "ca", "fix" or "fit")
#      value             the value
#      names             names of the model timeseries
#      pattern           a regular expression
#      period            the period
# RETURN: updated model object
#
#' @importFrom regts regts
set_values_ <- function(mdl, type, value, names, pattern, period) {
    if (!is.numeric(value)) {
        stop("argument value should be a numeric vector")
    }
    period <- as.regperiod_range(period)
    nper <- length_range(period)
    vlen <- length(value)
    if (vlen != 1 && vlen != nper) {
        stop(paste("Argument value should have length 1 or the same",
                   "length  as the number of periods"))
    }

    if (type == "data" || type == "fit") {
        data_names <- mdl@names
    } else {
        data_names <- mdl@ca_names
    }

    if (is.null(pattern) && is.null(names)) {
        names <- data_names
    } else if (is.null(names)) {
        names <- grep(pattern, data_names)
    } else if (!is.null(pattern)) {
        names <- union(names,  grep(pattern, data_names))
    }

    # TODO: if type = "data" or type = "ca" and value is a scalar, then the
    # code can be more efficient
    nvar <- length(names)
    data <- regts(matrix(value, nrow = nper, ncol = nvar),
                  period = period, names = names)
    return(set_data_(mdl, type, data, names, FALSE))
}

# General function used to change model data or constant adjustments.
# INPUT:
#      mdl               the model
#      type              type ("data" or "ca")
#      fun               the function
#      names             names of the model timeseries
#      pattern           a regular expression
#      period            the period
#      ...               arguments passed to fun
# RETURN: updated model object
#
change_values_ <- function(mdl, type, fun, names, pattern, period, ...) {

    period <- as.regperiod_range(period)

    if (type == "data") {
        data_names <- mdl@names
    } else {
        data_names <- mdl@ca_names
    }
    if (type == "data") {
        data_period <- mdl@data_period
    } else {
        data_period <- mdl@period
    }

    if (!missing(names)) {
        names <- intersect(data_names, names)
    }
    if (is.null(pattern) && is.null(names)) {
        names <- data_names
    } else if (is.null(names)) {
        names <- grep(pattern, data_names)
    } else if (!is.null(pattern)) {
        names <- union(names, grep(pattern, data_names))
    }

    period <- regrange_intersect(data_period, period)

    if (type == "data") {
        mdl@data[period, names] <- fun(mdl@data[period, names])
    } else {
        mdl@ca[period, names]   <- fun(mdl@ca[period, names])
    }
    return(invisible(mdl))
}
