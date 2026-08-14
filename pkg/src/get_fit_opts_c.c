#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "fit_options.h"
#include "init_set_get_options.h"
#include "get_fit_opts_c.h"
#include "get_fit_opts_fortran.h"
#include "option_builder.h"

#define N_OPTS 14

static SEXP get_debug_option(void);

SEXP get_fit_opts_c(SEXP model_index_) {

    /*
     * Extract fit options from Fortran memory
     */

    int model_index = asInteger(model_index_);

    F77_CALL(init_get_options)(&model_index);

    int maxiter, repopt, zero_ca, warn_ca, acc_jac, zealous, scale_method, 
        warn_zero_row, warn_zero_col, chkjac;
    double cvgabs, mkdcrt, cvgrel;
    F77_CALL(get_fit_options)(&maxiter, &cvgabs, &mkdcrt, &cvgrel, &zero_ca, 
                              &warn_ca, &repopt, &acc_jac, 
                              &zealous, &scale_method, 
                              &warn_zero_row, &warn_zero_col, &chkjac);

    /*
     * Create a named list with the options
     */
    OptionBuilder *b = ob_create(N_OPTS);

    ob_add_int    (b, "maxiter",  maxiter);
    ob_add_real   (b, "cvgabs",   cvgabs);
    ob_add_real   (b, "mkdcrt",   mkdcrt);
    ob_add_real   (b, "cvgrel",   cvgrel);

    ob_add_logical(b, "zero_ca",  zero_ca);
    ob_add_logical(b, "warn_ca",  warn_ca);
    ob_add_logical(b, "accurate_jac", acc_jac);
    ob_add_logical(b, "zealous",  zealous);

    ob_add_string (b, "scale_method",
                   get_fit_scale_method_text(scale_method));

    ob_add_logical(b, "warn_zero_row", warn_zero_row);
    ob_add_logical(b, "warn_zero_col", warn_zero_col);
    ob_add_logical(b, "chkjac",        chkjac);

    ob_add_string (b, "report", get_fit_repopt_text(repopt));
    ob_add_sexp   (b, "dbgopt",  get_debug_option());

    return ob_get(b); 
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
