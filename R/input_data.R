# This file contains documentation for the input data for various example
# models.

#' Example input for the ISLM model
#'
#' A \code{\link{regts}} containing example input data for a quarterly ISLM
#' model.
#' @format A \code{regts} with data for the period
#' \code{2010Q2/2011Q4} that can be used to solve the ISLM model
#' for the period \code{2010Q3/2011Q4} (the shift of the first period
#' is because of the lag 1).
"islm_input"
r  <- regts(3.35, start = "2015Q1", end = "2016Q3", labels = "interest rate")
y  <- regts(980,  start = "2015Q1", end = "2016Q3", labels = "income")
yd <- regts(790, start = "2015Q1", labels = "disposable income")
g  <- regts(210 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "government spending")
ms <- regts(200 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "money supply")
islm_input <- cbind(r, y, yd, g, ms)
devtools:::use_data(islm_input, overwrite = TRUE)




