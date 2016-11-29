library(isismdl)

copy_example_mdl("islm")
ret <- compile_mdl("islm.mdl")
mdl <- IsisMdl$new("islm.mif")
r  <- regts(3.35, start = "2015Q1", end = "2016Q3", labels = "interest rate")
y  <- regts(980,  start = "2015Q1", end = "2016Q3", labels = "income")
yd <- regts(790, start = "2015Q1", labels = "disposable income")
g  <- regts(210 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "government spending")
ms <- regts(200 * cumprod(rep(1.015, 6)), start = "2015Q2",
            labels = "money supply")
islm_input <- cbind(r, y, yd, g, ms)
mdl$set_period("2015Q2/2016Q3")
mdl$set_data(islm_input)
islm_input_mws <- mdl$get_mws()

devtools:::use_data(islm_input_mws, overwrite = TRUE)

# solve to check
mdl$solve()

# clean up stuff
unlink("islm.mdl")
unlink("islm.mif")
unlink("islm.mrf")
