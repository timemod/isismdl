#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "fit_options.h"
#include "get_option_utils.h"
#include "init_set_get_options.h"

#define N_OPTS 7

extern void F77_CALL(get_fit_options)(int *maxiter, double *cvgabs, 
              double *mkdcrt, int *zero_ca, int *warn_ca, int *repopt);
extern void F77_CALL(get_fit_dbgopts)(int *, int *, int *);

static SEXP get_debug_option(void);

SEXP get_fit_opts_c(SEXP mws_index_) {

    int mws_index = asInteger(mws_index_);

    F77_CALL(init_get_options)(&mws_index);

    init_options(N_OPTS);

    int maxiter, repopt, zero_ca, warn_ca;
    double cvgabs, mkdcrt;
    F77_CALL(get_fit_options)(&maxiter, &cvgabs, &mkdcrt, &zero_ca, &warn_ca,
                              &repopt);

    add_option("maxiter",  PROTECT(ScalarInteger(maxiter)));
    add_option("cvgabs",   PROTECT(ScalarReal(cvgabs)));
    add_option("mkdcrt",   PROTECT(ScalarReal(mkdcrt)));
    add_option("zero_ca",  PROTECT(ScalarLogical(zero_ca)));
    add_option("warn_ca",  PROTECT(ScalarLogical(warn_ca)));
    add_option("report",   PROTECT(mkString(get_fit_repopt_text(repopt))));

    add_option("dbgopt", get_debug_option());

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
