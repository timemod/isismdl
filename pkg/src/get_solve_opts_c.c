#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "solve_options.h"
#include "init_get_options.h"
#include "get_option_utils.h"

#define N_OPTS 21

extern void F77_CALL(get_solve_options)(int *imode, int *istart, int *maxit, 
              int *maxmat, double *rlxspeed, double *rlxmin, double *rlxmax, 
              double *cstpbk, double *cnmtrx, double *xrelax, int *mratex,
              int *uplead, int *erropt, int *repopt, int *ratrepopt, 
              int *ratreport_rep, int *ratfullreport_rep, int *bktmax, 
              double *xtfac, double *svdtest_tol);
extern void F77_CALL(get_solve_dbgopts)(int *, int *, int *,  int *, int *, 
                                      int *);
static SEXP get_debug_option(void);

SEXP get_solve_opts_c(SEXP mws_index_) {

    int mws_index = asInteger(mws_index_);

    F77_CALL(init_get_options)(&mws_index);

    init_options(N_OPTS);

    int imode, istart, maxit, maxmat, mratex, uplead, erropt, repopt, 
        ratrepopt, ratreport_rep, ratfullreport_rep, bktmax;
    double rlxspeed, rlxmin, rlxmax, cstpbk, cnmtrx, xrelax, xtfac, svdtest_tol;
    F77_CALL(get_solve_options)(&imode, &istart, &maxit, &maxmat, &rlxspeed,
                                &rlxmin, &rlxmax, &cstpbk, &cnmtrx, &xrelax,
                                &mratex, &uplead, &erropt, &repopt, &ratrepopt,
                                &ratreport_rep, &ratfullreport_rep, &bktmax, 
                                &xtfac, &svdtest_tol);

    add_option("mode",      PROTECT(mkString(get_mode_text(imode))));
    add_option("fbstart",   PROTECT(mkString(get_start_text(istart))));
    add_option("maxiter",   PROTECT(ScalarInteger(maxit)));
    add_option("maxjacupd", PROTECT(ScalarInteger(maxmat)));
    add_option("rlxspeed",  PROTECT(ScalarReal(rlxspeed)));
    add_option("rlxmin",    PROTECT(ScalarReal(rlxmin)));
    add_option("rlxmax",    PROTECT(ScalarReal(rlxmax)));
    add_option("cstpbk",    PROTECT(ScalarReal(cstpbk)));
    add_option("cnmtrx",    PROTECT(ScalarReal(cnmtrx)));
    add_option("bktmax",    PROTECT(ScalarReal(bktmax)));
    add_option("xrelax",    PROTECT(ScalarReal(xrelax)));
    add_option("xmaxiter",  PROTECT(ScalarInteger(mratex)));
    add_option("xupdate",   PROTECT(mkString(get_xupdate_text(uplead))));
    add_option("xtfac",     PROTECT(ScalarReal(xtfac)));
    add_option("svdtest_tol", PROTECT(ScalarReal(svdtest_tol)));
    add_option("erropt",    PROTECT(mkString(get_erropt_text(erropt))));
    add_option("report",    PROTECT(mkString(get_repopt_text(repopt))));
    add_option("ratreport", PROTECT(mkString(get_ratrepopt_text(ratrepopt))));
    add_option("ratreport_rep",  PROTECT(ScalarInteger(ratreport_rep)));
    add_option("ratfullreport_rep",  PROTECT(ScalarInteger(ratfullreport_rep)));

    add_option("dbgopt", get_debug_option());

    UNPROTECT(N_OPTS);
    return get_options();
}

SEXP get_debug_option(void) {

    int priter, prexen, jacprt, suptst, xsuptt, prscal;
    F77_CALL(get_solve_dbgopts)(&priter, &prexen, &jacprt, &suptst, &xsuptt, 
                                &prscal);

    if (!priter && !prexen && !jacprt && suptst && xsuptt && !prscal) {
        return PROTECT(mkString(DBG_NONE));
    } else if (priter && prexen && jacprt && !suptst && !xsuptt && prscal) {
        return PROTECT(mkString(DBG_ALL));
    } else {
        SEXP value = PROTECT(allocVector(STRSXP, 6));
        SET_STRING_ELT(value, 0, mkChar(DBG_PRITER_OPTS[priter]));
        SET_STRING_ELT(value, 1, mkChar(DBG_PREXEN_OPTS[prexen]));
        SET_STRING_ELT(value, 2, mkChar(DBG_JACPRT_OPTS[jacprt]));
        SET_STRING_ELT(value, 3, mkChar(DBG_SUPTST_OPTS[!suptst]));
        SET_STRING_ELT(value, 4, mkChar(DBG_XSUPTT_OPTS[!xsuptt]));
        SET_STRING_ELT(value, 5, mkChar(DBG_PRSCAL_OPTS[prscal]));
        return value;
    }
}
