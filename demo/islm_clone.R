library(regts)
library(isismdl)
mdl <- read_mdl("islm.mif")
mdl$set_mws(islm_input_mws)

mdl$solve()

mdl2 <- mdl$clone(deep = TRUE)

mdl2$set_param(list(c0 = 999))
mdl2$set_period("4000/4011")

mdl$get_param("c")
mdl2$get_param("c")

mdl$get_data()
mdl2$get_data()
