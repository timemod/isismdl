#include <Rinternals.h>
#include <Rdefines.h>
#include "fit_options.h"
#include "set_fit_options.h"
#include "init_set_get_options.h"

#define CHECK_LENGTH(name, value) if (length(value) > 1) { \
    error("The value for option %s should have length 1.", name); \
}

extern void F77_SUB(init_set_solve_opts)(int *mws_index, int *use_mws);
extern void F77_SUB(init_get_solve_opts)(int *mws_index);
extern void F77_SUB(set_fit_maxit)(int *);
extern void F77_SUB(set_fit_cvgabs)(double *);
extern void F77_SUB(set_fit_mkdcrt)(double *);
extern void F77_CALL(get_fit_dbgopts)(int *, int *, int *);
extern void F77_SUB(set_fit_dbgopts)(int *, int *, int *);
extern void F77_SUB(set_fit_repopt)(int *);

static void set_fit_option(const char *name, SEXP value);
static void set_fit_debug_opts(SEXP option);

void set_fit_opts_c(SEXP mws_index_, SEXP options) {
    int mws_index = asInteger(mws_index_);
    int opts_present = length(options) > 0;
    if (opts_present) {
        int use_mws = 1;
        set_fit_options(&mws_index, &use_mws, options);
    }
}

void set_fit_options(int *mws_index, int *use_mws, SEXP options) {

    F77_CALL(init_set_options)(mws_index, use_mws);

    /* call init_get_solve_opts, we need this because 
     * of the call of get_fit_dbgopts */
    F77_CALL(init_get_options)(mws_index);

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
        i = asInteger(value);
        F77_CALL(set_fit_maxit)(&i);
    } else if (!strcmp(name, "cvgabs")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_fit_cvgabs)(&x);
    } else if (!strcmp(name, "mkdcrt")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_fit_mkdcrt)(&x);
    } else if (!strcmp(name, "dbgopt")) {
        set_fit_debug_opts(value);
    } else if (!strcmp(name, "report")) {
        CHECK_LENGTH(name, value);
        i = get_fit_repopt(name, CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_fit_repopt)(&i);
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
        Rprintf("opt = %s\n", opt);
        int positive = strncmp(opt, "no", 2);
        const char *s = positive ? opt : opt + 2;
        if (!strcmp(s, FIT_PRICA_OPTS[1])) {
            prica = positive;
        } else if (!strcmp(s, FIT_PRIJAC_OPTS[1])) {
            prijac = positive;
        } else if (!strcmp(s, FIT_SUPSOT_OPTS[1])) {
            supsot = !positive;
        } else {
            error("Unknown debug option for the fit procedure %s\n",
                    opt);
        }
    }
    F77_CALL(set_fit_dbgopts)(&prica, &prijac, &supsot);
}
