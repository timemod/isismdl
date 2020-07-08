#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "fit_options.h"
#include "get_option_utils.h"
#include "init_set_get_options.h"
#include "get_fit_opts_c.h"

#define N_OPTS 14

extern void F77_CALL(get_fit_options)(int *maxiter, double *cvgabs, 
              double *mkdcrt, double *cvgrel, int *zero_ca, int *warn_ca, int *repopt,
              double *svdtest_tol, int *acc_jac, int *zealous, int *scale_method,
              int *warn_zero_col, int *chkjac);
extern void F77_CALL(get_fit_dbgopts)(int *, int *, int *);

static SEXP get_debug_option(void);

SEXP get_fit_opts_c(SEXP model_index_) {

    int model_index = asInteger(model_index_);

    F77_CALL(init_get_options)(&model_index);

    init_options(N_OPTS);

    int maxiter, repopt, zero_ca, warn_ca, acc_jac, zealous, scale_method, 
        warn_zero_col, chkjac;
    double cvgabs, mkdcrt, cvgrel, svdtest_tol;
    F77_CALL(get_fit_options)(&maxiter, &cvgabs, &mkdcrt, &cvgrel, &zero_ca, 
                              &warn_ca, &repopt, &svdtest_tol, &acc_jac, &zealous,
                              &scale_method, &warn_zero_col, &chkjac);

    add_option("maxiter",  PROTECT(ScalarInteger(maxiter)));
    add_option("cvgabs",   PROTECT(ScalarReal(cvgabs)));
    add_option("mkdcrt",   PROTECT(ScalarReal(mkdcrt)));
    add_option("cvgrel",   PROTECT(ScalarReal(cvgrel)));
    add_option("zero_ca",  PROTECT(ScalarLogical(zero_ca)));
    add_option("warn_ca",  PROTECT(ScalarLogical(warn_ca)));
    add_option("accurate_jac",  PROTECT(ScalarLogical(acc_jac)));
    add_option("zealous",  PROTECT(ScalarLogical(zealous)));
    add_option("scale_method",   PROTECT(mkString(get_fit_scale_method_text(scale_method))));
    add_option("warn_zero_col",  PROTECT(ScalarLogical(warn_zero_col)));
    add_option("chkjac",  PROTECT(ScalarLogical(chkjac)));
    add_option("report",   PROTECT(mkString(get_fit_repopt_text(repopt))));
    add_option("dbgopt", get_debug_option());
    add_option("svdtest_tol",   PROTECT(ScalarReal(svdtest_tol)));

    UNPROTECT(N_OPTS);
    return get_options();
}

static SEXP get_debug_option(void) {

    int prica, prijac, supsot;
    F77_CALL(get_fit_dbgopts)(&prica, &prijac, &supsot);

    SEXP value = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(value, 0, mkChar(FIT_PRICA_OPTS[prica]));
    SET_STRING_ELT(value, 1, mkChar(FIT_PRIJAC_OPTS[prijac]));
    SET_STRING_ELT(value, 2, mkChar(FIT_SUPSOT_OPTS[supsot]));
    return value;
}
