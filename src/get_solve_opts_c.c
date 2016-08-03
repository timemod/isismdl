#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "solve_options.h"

#define N_OPTS 12

extern void F77_SUB(init_get_solve_opts)(int *mws_index);
extern void F77_CALL(get_solve_options)(int *imode, int *istart, int *maxit, 
              int *maxmat, double *rlxspeed, double *rlxmin, double *rlxmax, 
              double *cstpbk, double *cnmtrx, double *xrelax, int *mratex,
              int *uplead);
static void add_option(const char *name, SEXP value);

static SEXP ret, names;
static int cnt;

SEXP get_solve_opts_c(SEXP mws_index_) {

    int mws_index = asInteger(mws_index_);

    F77_CALL(init_get_solve_opts)(&mws_index);

    ret  = PROTECT(allocVector(VECSXP, N_OPTS));
    names = PROTECT(allocVector(STRSXP, N_OPTS));

    cnt = 0;

    int imode, istart, maxit, maxmat, mratex, uplead;
    double rlxspeed, rlxmin, rlxmax, cstpbk, cnmtrx, xrelax;
    F77_CALL(get_solve_options)(&imode, &istart, &maxit, &maxmat, &rlxspeed,
                                &rlxmin, &rlxmax, &cstpbk, &cnmtrx, &xrelax,
                                &mratex, &uplead);

    add_option("mode",      PROTECT(mkString(get_mode_text(imode))));
    add_option("fbstart",   PROTECT(mkString(get_start_text(istart))));
    add_option("maxiter",   PROTECT(ScalarInteger(maxit)));
    add_option("maxjacupd", PROTECT(ScalarInteger(maxmat)));
    add_option("rlxspeed",  PROTECT(ScalarReal(rlxspeed)));
    add_option("rlxmin",    PROTECT(ScalarReal(rlxmin)));
    add_option("rlxmax",    PROTECT(ScalarReal(rlxmax)));
    add_option("cstpbk",    PROTECT(ScalarReal(cstpbk)));
    add_option("cnmtrx",    PROTECT(ScalarReal(cnmtrx)));
    add_option("xrelax",    PROTECT(ScalarReal(xrelax)));
    add_option("xmaxiter",  PROTECT(ScalarInteger(mratex)));
    add_option("xupdate",   PROTECT(mkString(get_xupdate_text(uplead))));

    setAttrib(ret, R_NamesSymbol, names);

    UNPROTECT(2 + N_OPTS);
    return ret;
}

static void add_option(const char *name, SEXP value) {
    SET_STRING_ELT(names, cnt, mkChar(name));
    SET_VECTOR_ELT(ret, cnt++, value);
}
