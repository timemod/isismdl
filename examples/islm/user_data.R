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
mdl <- isis_mdl("islm.mdl", data = islm_input, silent = FALSE)
print(mdl)

# add labels for model variables not in the input data
mdl$set_labels(c(i = "investment", c = "consumption", md = "money demand",
                 t = "tax"))


mdl$solve()

print(mdl$get_user_data())
#print(mdl$get_user_data("investment"))
mdl$set_user_data(hallo =  "government spending")
print(mdl$get_user_data())

mdl$get_user_data("hallo")

rds_file <- tempfile()
mdl$write_mdl(rds_file)
mdl2 <- read_mdl(rds_file)
print(mdl2$get_user_data())
