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

# General function used to update model data, constant adjustments, fix values
# or fit value.s
# INPUT:
#      mdl               the model
#      type              type ("data", "ca", "fix" or "fit")
#      new_data          new timeseries data
#      new_names         names of the new timeseries data
#      new_names_missing TRUE if the names of the new timeseries has not been
#                        specicified
#  RETURN: updated old_data
#
update_data <- function(mdl, type, new_data, new_names, new_names_missing) {

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
        if (length(new_names) < NCOL(data)) {
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
        mdl@data[p, names] <- new_data
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
