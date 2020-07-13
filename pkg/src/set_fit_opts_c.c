#include <Rinternals.h>
#include <Rdefines.h>
#include "fit_options.h"
#include "set_fit_options.h"
#include "set_option_utils.h"
#include "init_set_get_options.h"
#include "set_fit_opts_c.h"

extern void F77_SUB(init_set_solve_opts)(int *model_index, int *use_mws);
extern void F77_SUB(init_get_solve_opts)(int *model_index);
extern void F77_SUB(set_fit_maxit)(int *);
extern void F77_SUB(set_fit_cvgabs)(double *);
extern void F77_SUB(set_fit_mkdcrt)(double *);
extern void F77_SUB(set_fit_cvgrel)(double *);
extern void F77_SUB(set_fit_zero_ca)(int *);
extern void F77_SUB(set_fit_warn_ca)(int *);
extern void F77_SUB(set_fit_accurate_jac)(int *);
extern void F77_SUB(set_fit_zealous)(int *);
extern void F77_SUB(set_fit_warn_zero_row)(int *);
extern void F77_SUB(set_fit_warn_zero_col)(int *);
extern void F77_SUB(get_fit_dbgopts)(int *, int *, int *);
extern void F77_SUB(set_fit_dbgopts)(int *, int *, int *);
extern void F77_SUB(set_fit_repopt)(int *);
extern void F77_SUB(set_fit_scale_method)(int *);
extern void F77_SUB(set_fit_svdtest_tol)(double *);
extern void F77_SUB(set_fit_chkjac)(int *);


static void set_fit_option(const char *name, SEXP value);
static void set_fit_debug_opts(SEXP option);

SEXP set_fit_opts_c(SEXP model_index_, SEXP options) {
    int model_index = asInteger(model_index_);
    int opts_present = length(options) > 0;
    if (opts_present) {
        int use_mws = 1;
        F77_CALL(init_set_options)(&model_index, &use_mws);
        set_fit_options(&model_index, options);
    }
    return R_NilValue;
}

void set_fit_options(int *model_index, SEXP options) {

    /* call init_get_solve_opts, we need this because 
     * of the call of get_fit_dbgopts */
    F77_CALL(init_get_options)(model_index);

    SEXP names = getAttrib(options, R_NamesSymbol);
    int i;
    for (i = 0; i < length(options); i++) {
        set_fit_option(CHAR(STRING_ELT(names, i)), VECTOR_ELT(options, i));
    }
}

static void set_fit_option(const char *name, SEXP value) {
    int i;
    double x;
    if (!strcmp(name, "maxiter")) {
        CHECK_LENGTH(name, value);
        i = get_positive_int(name, value);
        F77_CALL(set_fit_maxit)(&i);
    } else if (!strcmp(name, "cvgabs")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        F77_CALL(set_fit_cvgabs)(&x);
    } else if (!strcmp(name, "mkdcrt")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        F77_CALL(set_fit_mkdcrt)(&x);
    } else if (!strcmp(name, "cvgrel")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        F77_CALL(set_fit_cvgrel)(&x);
    } else if (!strcmp(name, "zero_ca")) {
        CHECK_LENGTH(name, value);
        i = get_logical(name, value);
        F77_CALL(set_fit_zero_ca)(&i);
    } else if (!strcmp(name, "warn_ca")) {
        CHECK_LENGTH(name, value);
        i = get_logical(name, value);
        F77_CALL(set_fit_warn_ca)(&i);
    } else if (!strcmp(name, "accurate_jac")) {
        CHECK_LENGTH(name, value);
        i = get_logical(name, value);
        F77_CALL(set_fit_accurate_jac)(&i);
    } else if (!strcmp(name, "zealous")) {
        CHECK_LENGTH(name, value);
        i = get_logical(name, value);
        F77_CALL(set_fit_zealous)(&i);
    } else if (!strcmp(name, "warn_zero_row")) {
        CHECK_LENGTH(name, value);
        i = get_logical(name, value);
        F77_CALL(set_fit_warn_zero_row)(&i);
    } else if (!strcmp(name, "warn_zero_col")) {
        CHECK_LENGTH(name, value);
        i = get_logical(name, value);
        F77_CALL(set_fit_warn_zero_col)(&i);
    } else if (!strcmp(name, "chkjac")) {
        CHECK_LENGTH(name, value);
        i = get_logical(name, value);
        F77_CALL(set_fit_chkjac)(&i);
    } else if (!strcmp(name, "dbgopt")) {
        set_fit_debug_opts(value);
    } else if (!strcmp(name, "report")) {
        CHECK_LENGTH(name, value);
        i = get_fit_repopt(name, CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_fit_repopt)(&i);
    } else if (!strcmp(name, "svdtest_tol")) {
        CHECK_LENGTH(name, value);
        x = get_finite_number(name, value);
        F77_CALL(set_fit_svdtest_tol)(&x);
    } else if (!strcmp(name, "scale_method")) {
        CHECK_LENGTH(name, value);
        i = get_fit_scale_method(name, CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_fit_scale_method)(&i);
    } else {
       error("Unknown fit option %s\n", name);
    }
}

static void set_fit_debug_opts(SEXP option) {

    int prica, prijac, supsot;
    F77_CALL(get_fit_dbgopts)(&prica, &prijac, &supsot);
    int i;
    for (i = 0; i < length(option); i++) {
        const char *opt = CHAR(STRING_ELT(option, i));
        int positive = strncmp(opt, "no", 2);
        const char *s = positive ? opt : opt + 2;
        if (!strcmp(s, FIT_PRICA_OPTS[1])) {
            prica = positive;
        } else if (!strcmp(s, FIT_PRIJAC_OPTS[1])) {
            prijac = positive;
        } else if (!strcmp(s, FIT_SUPSOT_OPTS[1])) {
            supsot = positive;
        } else {
            error("Unknown debug option for the fit procedure %s\n",
                    opt);
        }
    }
    F77_CALL(set_fit_dbgopts)(&prica, &prijac, &supsot);
}
