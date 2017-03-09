library(isismdl)

# prepare data
r  <- regts(3.35, start = "2015Q1", end = "2016Q3", labels = "interest rate")
y  <- regts(980,  start = "2015Q1", end = "2016Q3", labels = "income")
yd <- regts(790, start = "2015Q1", labels = "disposable income")
g  <- regts(210 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "government spending")
ms <- regts(200 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "money supply")
islm_input <- cbind(r, y, yd, g, ms)

# create the model
mdl <- isis_mdl("islm.mdl", data = islm_input);

# add labels for model variables not in the input data
mdl$set_labels(c(i = "investment", c = "consumption", md = "money demand",
                 t = "tax"))

mdl$solve()

mdl$write_mdl("islm_basis.rds")
