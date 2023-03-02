library(isismdl)
library(testthat)
library(readr)


capture_output(mdl <- isis_mdl("mdl/ulfunc1.mdl", period = "2010"))
mdl$set_values(3, names = "x")
mdl$set_values(2, names = "y")

test_that("solve gives an error", {
  msg <- "Calling R function not supported yet"
  expect_error(mdl$solve(options = list(report = "none")), msg)
})

# TODO: when calling R functions has been implemented, do all the test
# done in isisv5/testmdl/error/jobs
# The original error code follows below:

# (* error: number of arguments not correct *)
#
# eta = procedure(return : number;
#                 in x : number;
#                 in y : number;
#                 in z : number;
#
#                 return = x * y * 10;
# );
#
# solve;
# primdt;
#
# (* error: incorrect argument type *)
#
# eta = procedure(return : number;
#                 in x : number;
#                 in y : text;
#
#                 return = x * txtnum(y) * 10;
# );
#
# solve;
# primdt;
#
# (* error: incorrect return type *)
#
# eta = procedure(return : number;
#                 in x : number;
#                 in y : number;
#
#                 return = [(x * y * 10)];
# );
#
# solve;
# primdt;

# (* error: reading a model inside a user language procedure *)
#
# eta = procedure(return : number;
#                 in x : number;
#                 in y : number;
#
#                 return = x * y * 10;
#                 readmdl("../mdl/ulfunc");
# );
#
# cmpmdl("../mdl/ulfunc");
# readmdl("../mdl/ulfunc");
# setmdp(2010y);
# x = 3;
# y = 2;
# solve;
# primdt;
