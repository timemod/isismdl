#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "solve_options.h"

#define N_OPTS 3

extern void F77_SUB(init_get_solve_opts)(int *mws_index);
extern void F77_SUB(get_solve_options)(int *, int *, int *);
static void add_option(const char *name, SEXP value);

static SEXP ret, names;
static int cnt;

SEXP get_solve_opts_c(SEXP mws_index_) {

    int mws_index = asInteger(mws_index_);

    F77_CALL(init_get_solve_opts)(&mws_index);

    ret  = PROTECT(allocVector(VECSXP, N_OPTS));
    names = PROTECT(allocVector(STRSXP, N_OPTS));

    cnt = 0;

    int imode, istart, maxit;
    F77_CALL(get_solve_options)(&imode, &istart, &maxit);

    add_option("mode", PROTECT(mkString(get_mode_text(imode))));
    add_option("fbstart", PROTECT(mkString(get_start_text(istart))));
    add_option("maxiter", PROTECT(ScalarInteger(maxit)));

    setAttrib(ret, R_NamesSymbol, names);

    UNPROTECT(2 + N_OPTS);
    return ret;
}

static void add_option(const char *name, SEXP value) {
    SET_STRING_ELT(names, cnt, mkChar(name));
    SET_VECTOR_ELT(ret, cnt++, value);
}
