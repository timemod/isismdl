#' Function solve_mdl solves model for given data and returns resulting data and constant adjustments (ca).
#' @export
solve_mdl <- function(model_file, data, period, fix_values, ca, fit_targets) {
    if (missing(model_file) || missing(data)) {
        stop("Please specify at least model_file and data.")
    }
    
    # Write output to temp file
    temp <- file()
	on.exit({sink(); close(temp)}, add = TRUE)
    sink(temp)
    
    # Do the work
    mdl <- isis_mdl(model_file = model_file, data = data)
    
    if (!missing(period)) mdl$set_period(period)
    if (!missing(ca)) mdl$set_ca(ca)
    if (!missing(fix_values)) mdl$set_fix(fix_values)
    if (!missing(fit_targets)) mdl$set_fit(fit_targets)
    
    mdl$fill_mdl_data()
    mdl$solve()

    list(data = mdl$get_data()[period, drop = F], ca = mdl$get_ca()[period, drop = F])
}