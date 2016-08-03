#include <Rinternals.h>
#include <Rdefines.h>
#include "set_solve_opts.h"
#include "solve_options.h"
#include "set_solve_options.h"

#define CHECK_LENGTH(name, value) if (length(value) > 1) { \
    error("The value for option %s should have length 1.", name); \
}

extern void F77_SUB(init_set_solve_opts)(int *mws_index, int *use_mws);
extern void F77_SUB(set_mode)(int *);
extern void F77_SUB(set_start)(int *);
extern void F77_SUB(set_maxit)(int *);
extern void F77_SUB(set_maxmat)(int *);
extern void F77_SUB(set_rlxspeed)(double *);
extern void F77_SUB(set_rlxmin)(double *);
extern void F77_SUB(set_rlxmax)(double *);
extern void F77_SUB(set_cstpbk)(double *);
extern void F77_SUB(set_cnmtrx)(double *);
extern void F77_SUB(set_xrelax)(double *);
extern void F77_SUB(set_mratex)(int *);
extern void F77_SUB(set_uplead)(int *);

static void set_option(const char *name, SEXP value);

void set_solve_opts_c(SEXP mws_index_, SEXP options) {
    int mws_index = asInteger(mws_index_);
    int opts_present = length(options) > 0;
    if (opts_present) {
        int use_mws = 1;
        set_solve_options(&mws_index, &use_mws, options);
    }
}

void set_solve_options(int *mws_index, int *use_mws, SEXP options) {

    F77_CALL(init_set_solve_opts)(mws_index, use_mws);

    SEXP names = getAttrib(options, R_NamesSymbol);
    int i;
    for (i = 0; i < length(options); i++) {
        set_option(CHAR(STRING_ELT(names, i)), VECTOR_ELT(options, i));
    }
}

static void set_option(const char *name, SEXP value) {
    int i;
    double x;
    if (!strcmp(name, "mode")) {
        CHECK_LENGTH(name, value);
        i = get_imode(CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_mode)(&i);
    } else if (!strcmp(name, "fbstart")) {
        CHECK_LENGTH(name, value);
        i = get_istart(CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_start)(&i);
    } else if (!strcmp(name, "maxiter")) {
        CHECK_LENGTH(name, value);
        i = asInteger(value);
        F77_CALL(set_maxit)(&i);
    } else if (!strcmp(name, "maxjacupd")) {
        CHECK_LENGTH(name, value);
        i = asInteger(value);
        F77_CALL(set_maxmat)(&i);
    } else if (!strcmp(name, "rlxspeed")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_rlxspeed)(&x);
    } else if (!strcmp(name, "rlxmin")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_rlxmin)(&x);
    } else if (!strcmp(name, "rlxmax")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_rlxmax)(&x);
    } else if (!strcmp(name, "cstpbk")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_cstpbk)(&x);
    } else if (!strcmp(name, "cnmtrx")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_cnmtrx)(&x);
    } else if (!strcmp(name, "xrelax")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_xrelax)(&x);
    } else if (!strcmp(name, "xmaxiter")) {
        CHECK_LENGTH(name, value);
        i = asInteger(value);
        F77_CALL(set_mratex)(&i);
    } else if (!strcmp(name, "xupdate")) {
        CHECK_LENGTH(name, value);
        i = get_uplead(CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_uplead)(&i);
    } else {
       error("Unknown solve option %s\n", name);
    }
}
