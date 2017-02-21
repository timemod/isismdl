prepare_mws <- function(model_index, period, data, ca =  NULL, fix = NULL,
                        fit = NULL) {
    set_period(model_index, period)
    set_var(model_index, 1L, data, period)
    if (!is.null(ca)) {
        set_var(model_index, 2L, ca, period)
    }
    if (!is.null(fix)) {
        set_var(model_index, 3L, fix, period)
    }
    if (!is.null(fit)) {
        set_var(model_index, 4L, fit, period)
    }
}

set_period <- function(model_index, period) {
    period <- as.regperiod_range(period)
    p1 <- start_period(period)
    p2 <- end_period(period)
    start <- as.integer(c(get_year(p1), get_subperiod(p1)))
    end   <- as.integer(c(get_year(p2), get_subperiod(p2)))
    retval <- .Fortran("set_period_fortran", model_index = model_index,
                       start = start, end = end, freq  = as.integer(period[3]),
                       ier = 1L)
    return(invisible(NULL))
}

set_var <- function(model_index, set_type, data, period) {
    # General function used to update model data, constant adjustments,
    # fix values or fit targets.
    if (NCOL(data) == 0) {
        # TODO: warning?
        return (invisible(NULL))
    }
    # if (is.null(names)) {
    #     if (names_missing) {
    #         stop(paste("Argument data has no colnames.",
    #                    "In that case, argument names should be specified"))
    #     } else {
    #         stop("names is null")
    #     }
    # }  else {
    #     if (length(names) < NCOL(data)) {
    #         stop("The length of arument names is less than the number of columns of data")
    #     }
    # }
    data <- as.regts(data)
    shift <- get_period_indices(get_regperiod_range(data), period)$startp
    if (!is.matrix(data)) {
        dim(data) <- c(length(data), 1)
    }
    if (is.integer(data) || !is.numeric(data)) {
        # make sure that data is a matrix of numeric values
        data <- apply(data, MARGIN = c(1,2), FUN = as.numeric)
    }
#
#     if (set_type == 1) {
#         lbls <- ts_labels(data)
#         if (!is.null(lbls)) {
#             names(lbls) <- names
#             private$update_labels(lbls)
#         }
#     }
    names <- colnames(data)
    .Call(set_c, set_type, model_index, data, names, shift)

    # TODO: report number of timeseries that have not been set
    return(invisible(NULL))
}


get_period_indices <- function(period, model_period, extended = TRUE) {
    period <- as.regperiod_range(period)
    mdl_period_start <- start_period(model_period)
    startp <- as.integer(start_period(period) - mdl_period_start + 1)
    endp   <- as.integer(end_period(period)   - mdl_period_start + 1)
    return(list(startp = startp, endp = endp))
}
