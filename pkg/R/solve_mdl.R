#' Function solve_mdl returns a solved model, given a model file name and corresponding data.
#' @export
solve_mdl <- function(model_file, period, data, ca, fix_values, fit_targets) {
    if (missing(model_file) || missing(period) || missing(data)) {
        stop("Please specify at least model_file, period and data.")
    }
	
	library(isismdl)
	
    mdl <- isis_mdl(modelname = model_file, period = period, data = data)
	
	if (!missing(ca)) mdl$set_ca(ca)
	if (!missing(fix_values)) mdl$set_fix(fixed_ts)
	if (!missing(fit_targets)) mdl$set_fit(fit_targets)
		
	mdl$fill_mdl_data()
	mdl$solve(period)

    return(mdl)
}