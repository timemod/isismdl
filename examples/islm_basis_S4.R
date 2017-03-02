library(isismdl)

period <- regperiod_range("2015Q2", "2016Q3")

mdl <- isis_mdl_S4("islm", period)

r  <- regts(3.35, start = "2015Q1", end = "2016Q3", labels = "interest rate")
y  <- regts(980,  start = "2015Q1", end = "2016Q3", labels = "income")
yd <- regts(790, start = "2015Q1", labels = "disposable income")
g  <- regts(210 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "government spending")
ms <- regts(200 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "money supply")
islm_input <- cbind(r, y, yd, g, ms)

mdl <- set_data(mdl, islm_input)

mdl  <- solve_mdl(mdl)
print(mdl)

saveRDS(mdl, "islm_basis_S4.rds")
