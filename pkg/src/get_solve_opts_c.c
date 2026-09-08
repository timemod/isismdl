#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "solve_options.h"
#include "init_get_options.h"
#include "get_solve_opts_c.h"
#include "get_solve_opts_fortran.h"
#include "option_builder.h"

#define N_OPTS 21

static SEXP get_debug_option(void);

SEXP get_solve_opts_c(SEXP model_index_) {

    /* 
     * Extract option from Fortran memory 
    */
    int model_index = asInteger(model_index_);

    F77_CALL(init_get_options)(&model_index);

    int imode, istart, maxit, maxmat, mratex, uplead, erropt, repopt, 
        ratrepopt, ratreport_rep, ratfullreport_rep, bktmax, store_jac;
    double rlxspeed, rlxmin, rlxmax, cstpbk, cnmtrx, xrelax, xtfac;
    F77_CALL(get_solve_options)(&imode, &istart, &maxit, &maxmat, &rlxspeed,
                                &rlxmin, &rlxmax, &cstpbk, &cnmtrx, &xrelax,
                                &mratex, &uplead, &erropt, &repopt, &ratrepopt,
                                &ratreport_rep, &ratfullreport_rep, &bktmax, 
                                &xtfac, &store_jac);
    /*
     * Create a named list with the options
     */
    OptionBuilder *b = ob_create(N_OPTS);
    ob_add_string (b, "mode",              get_mode_text(imode));
    ob_add_string (b, "fbstart",           get_start_text(istart));
    ob_add_int    (b, "maxiter",           maxit);
    ob_add_int    (b, "maxjacupd",         maxmat);
    ob_add_real   (b, "rlxspeed",          rlxspeed);
    ob_add_real   (b, "rlxmin",            rlxmin);
    ob_add_real   (b, "rlxmax",            rlxmax);
    ob_add_real   (b, "cstpbk",            cstpbk);
    ob_add_real   (b, "cnmtrx",            cnmtrx);
    ob_add_real   (b, "bktmax",            bktmax);
    ob_add_real   (b, "xrelax",            xrelax);
    ob_add_int    (b, "xmaxiter",          mratex);
    ob_add_string (b, "xupdate",           get_xupdate_text(uplead));
    ob_add_real   (b, "xtfac",             xtfac);
    ob_add_string (b, "erropt",            get_erropt_text(erropt));
    ob_add_string (b, "report",            get_repopt_text(repopt));
    ob_add_string (b, "ratreport",         get_ratrepopt_text(ratrepopt));
    ob_add_int    (b, "ratreport_rep",     ratreport_rep);
    ob_add_int    (b, "ratfullreport_rep", ratfullreport_rep);
    ob_add_sexp   (b, "dbgopt",            get_debug_option());
    ob_add_logical(b, "store_jac",         store_jac);

    return ob_get(b);
}

SEXP get_debug_option(void) {

    int priter, prexen, jacprt, suptst, xsuptt, prscal;
    F77_CALL(get_solve_dbgopts)(&priter, &prexen, &jacprt, &suptst, &xsuptt, 
                                &prscal);

    if (!priter && !prexen && !jacprt && suptst && xsuptt && !prscal) {
        return mkString(DBG_NONE);
    } else if (priter && prexen && jacprt && !suptst && !xsuptt && prscal) {
        return mkString(DBG_ALL);
    } else {
        SEXP value = PROTECT(allocVector(STRSXP, 6));
        SET_STRING_ELT(value, 0, mkChar(DBG_PRITER_OPTS[priter]));
        SET_STRING_ELT(value, 1, mkChar(DBG_PREXEN_OPTS[prexen]));
        SET_STRING_ELT(value, 2, mkChar(DBG_JACPRT_OPTS[jacprt]));
        SET_STRING_ELT(value, 3, mkChar(DBG_SUPTST_OPTS[!suptst]));
        SET_STRING_ELT(value, 4, mkChar(DBG_XSUPTT_OPTS[!xsuptt]));
        SET_STRING_ELT(value, 5, mkChar(DBG_PRSCAL_OPTS[prscal]));
        UNPROTECT(1);
        return value;
    }
}
