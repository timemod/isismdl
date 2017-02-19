library(isismdl)

period <- regperiod_range("2015Q2", "2016Q3")
mif_file <- "islm.mif"
mws_file <- "basis_mws.RData"

mdl <- compile_mdl("islm")
mdl$set_period(period)

# prepare data
r  <- regts(3.35, start = "2015Q1", end = "2016Q3", labels = "interest rate")
y  <- regts(980,  start = "2015Q1", end = "2016Q3", labels = "income")
yd <- regts(790, start = "2015Q1", labels = "disposable income")
g  <- regts(210 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "government spending")
ms <- regts(200 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "money supply")
islm_input <- cbind(r, y, yd, g, ms)
mdl$set_data(islm_input)

mdl$set_labels(c(i = "investment", c = "consumption", md = "money demand",
                 t = "tax"))

mdl$solve()

mdl$saveRDS("islm_basis.rds")

#basis_mws <- mdl$get_mws()
#save(basis_mws, file = mws_file)
