#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "fit_options.h"
#include "get_option_utils.h"
#include "init_set_get_options.h"
#include "get_fit_opts_c.h"
#include "get_fit_opts_fortran.h"

#define N_OPTS 14

static SEXP get_debug_option(void);

SEXP get_fit_opts_c(SEXP model_index_) {

    int model_index = asInteger(model_index_);

    F77_CALL(init_get_options)(&model_index);

    init_options(N_OPTS);

    int maxiter, repopt, zero_ca, warn_ca, acc_jac, zealous, scale_method, 
        warn_zero_row, warn_zero_col, chkjac;
    double cvgabs, mkdcrt, cvgrel;
    F77_CALL(get_fit_options)(&maxiter, &cvgabs, &mkdcrt, &cvgrel, &zero_ca, 
                              &warn_ca, &repopt, &acc_jac, 
                              &zealous, &scale_method, 
                              &warn_zero_row, &warn_zero_col, &chkjac);

    add_option("maxiter",  PROTECT(ScalarInteger(maxiter)));
    add_option("cvgabs",   PROTECT(ScalarReal(cvgabs)));
    add_option("mkdcrt",   PROTECT(ScalarReal(mkdcrt)));
    add_option("cvgrel",   PROTECT(ScalarReal(cvgrel)));
    add_option("zero_ca",  PROTECT(ScalarLogical(zero_ca)));
    add_option("warn_ca",  PROTECT(ScalarLogical(warn_ca)));
    add_option("accurate_jac",  PROTECT(ScalarLogical(acc_jac)));
    add_option("zealous",  PROTECT(ScalarLogical(zealous)));
    add_option("scale_method", 
               PROTECT(mkString(get_fit_scale_method_text(scale_method))));
    add_option("warn_zero_row", PROTECT(ScalarLogical(warn_zero_row)));
    add_option("warn_zero_col", PROTECT(ScalarLogical(warn_zero_col)));
    add_option("chkjac", PROTECT(ScalarLogical(chkjac)));
    add_option("report", PROTECT(mkString(get_fit_repopt_text(repopt))));
    add_option("dbgopt", PROTECT(get_debug_option()));

    UNPROTECT(N_OPTS);
    return get_options();
}

static SEXP get_debug_option(void) {

    int prica, prijac, store_jac, supsot;
    F77_CALL(get_fit_dbgopts)(&prica, &prijac, &store_jac, &supsot);

    SEXP value = PROTECT(allocVector(STRSXP, 4));
    SET_STRING_ELT(value, 0, mkChar(FIT_PRICA_OPTS[prica]));
    SET_STRING_ELT(value, 1, mkChar(FIT_PRIJAC_OPTS[prijac]));
    SET_STRING_ELT(value, 2, mkChar(FIT_STORE_JAC_OPTS[store_jac]));
    SET_STRING_ELT(value, 3, mkChar(FIT_SUPSOT_OPTS[supsot]));
    UNPROTECT(1);
    return value;
}
