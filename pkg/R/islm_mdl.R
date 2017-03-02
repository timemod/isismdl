#' Returns an example ISLM model
#'
#' This function returns an example ISLM model, If argument \code{period} has
#' been specified, then this function also generates some example data for
#' the feedback variables, lags and exogenous variables. The model returned
#' is ready to be solved.
#'
#' @param period the model period for the ISLM model
#' @return a \code{\link{IsisMdl}} object
#' @examples
#' mdl <- islm_mdl()
#' @export
islm_mdl <- function(period = NULL) {
    mdl_file <- tempfile(fileext = ".mdl")
    copy_example_mdl("islm", mdl_file)
    mdl <- isis_mdl(mdl_file)
    unlink(mdl_file)

    if (!is.null(period)) {
        period <- as.regperiod_range(period)
        mdl$set_period(period)
        nper <- length_range(period)
        data_per <- mdl$get_data_period()
        r  <- regts(3.35, period = data_per, labels = "interest rate")
        y  <- regts(980,  period = data_per, labels = "income")
        yd <- regts(790, start = start_period(data_per), labels = "disposable income")
        g  <- regts(210 * cumprod(rep(1.015, nper)), period = period,
                    labels = "government spending")
        ms <- regts(200 * cumprod(rep(1.015, nper)), period = period,
                     labels = "money supply")
        islm_input <- cbind(r, y, yd, g, ms)
        mdl$set_data(islm_input)
        mdl$set_labels(c(i = "investment", c = "consumption", md = "money demand",
                         t = "tax"))
    }
    return(mdl)
}