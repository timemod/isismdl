.libPaths("/usr/local/lib/R/cpblib/R_4.0/cpb_6")
print(.libPaths())

library(isismdl)

mdl <- isis_mdl("../mdl/capitals.mdl", period = "2018/2020")

data = mdl$get_data()
data[] <- seq_along(data)
mdl$set_data(data)
print(mdl$get_data())

mdl$write_mdl("capitals.rds")
