#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "solve_options.h"

#define N_OPTS 13

extern void F77_SUB(init_get_solve_opts)(int *mws_index);
extern void F77_CALL(get_solve_options)(int *imode, int *istart, int *maxit, 
              int *maxmat, double *rlxspeed, double *rlxmin, double *rlxmax, 
              double *cstpbk, double *cnmtrx, double *xrelax, int *mratex,
              int *uplead);
extern void F77_CALL(get_solve_dbgops)(int *, int *, int *,  int *, int *, 
                                      int *);
static void add_option(const char *name, SEXP value);
static void add_debug_option(void);

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

    add_debug_option();

    setAttrib(ret, R_NamesSymbol, names);

    UNPROTECT(2 + N_OPTS);
    return ret;
}

static void add_option(const char *name, SEXP value) {
    SET_STRING_ELT(names, cnt, mkChar(name));
    SET_VECTOR_ELT(ret, cnt++, value);
}

static void add_debug_option(void) {

    SET_STRING_ELT(names, cnt, mkChar("dbgopt"));

    int priter, prexen, jacprt, suptst, xsuptt, prscal;
    F77_CALL(get_solve_dbgopts)(&priter, &prexen, &jacprt, &suptst, &xsuptt, 
                                &prscal);

    if (!priter && !prexen && !jacprt && !suptst && !xsuptt && !prscal) {
        SET_VECTOR_ELT(ret, cnt++, PROTECT(mkString(DBG_NONE)));
    } else if (priter && prexen && jacprt && suptst && xsuptt && prscal) {
        SET_VECTOR_ELT(ret, cnt++, PROTECT(mkString(DBG_ALL)));
    } else {
        SEXP value = PROTECT(allocVector(STRSXP, 6));
        SET_STRING_ELT(value, 0, mkChar(DBG_PRITER_OPTS[priter]));
        SET_STRING_ELT(value, 1, mkChar(DBG_PREXEN_OPTS[prexen]));
        SET_STRING_ELT(value, 2, mkChar(DBG_JACPRT_OPTS[jacprt]));
        SET_STRING_ELT(value, 3, mkChar(DBG_SUPTST_OPTS[suptst]));
        SET_STRING_ELT(value, 4, mkChar(DBG_XSUPTT_OPTS[xsuptt]));
        SET_STRING_ELT(value, 5, mkChar(DBG_PRSCAL_OPTS[prscal]));
        SET_VECTOR_ELT(ret, cnt++, value);
    }
}
