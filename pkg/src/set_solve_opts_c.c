#include <Rinternals.h>
#include <Rdefines.h>
#include "set_solve_opts.h"
#include "solve_options.h"
#include "set_solve_options.h"
#include "set_option_utils.h"
#include "init_set_get_options.h"

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
extern void F77_CALL(get_solve_dbgopts)(int *, int *, int *,  int *, int *, 
                                      int *);
extern void F77_SUB(set_solve_dbgopts)(int *, int *, int *, int *, int *, int *);
extern void F77_SUB(set_erropt)(int *);
extern void F77_SUB(set_repopt)(int *);
extern void F77_SUB(set_ratrepopt)(int *);
extern void F77_SUB(set_ratreport_rep)(int *);
extern void F77_SUB(set_ratfullreport_rep)(int *);
extern void F77_SUB(set_bktmax)(int *);
extern void F77_SUB(set_xtfac)(double *);

extern void F77_SUB(check_options)(void);

static void set_option(const char *name, SEXP value);
static void set_debug_opts(SEXP option);

void set_solve_opts_c(SEXP mws_index_, SEXP options) {
    int mws_index = asInteger(mws_index_);
    int opts_present = length(options) > 0;
    if (opts_present) {
        int use_mws = 1;
        F77_CALL(init_set_options)(&mws_index, &use_mws);
        set_solve_options(&mws_index, options);
    }
}

void set_solve_options(int *mws_index, SEXP options) {

    /* call init_get_solve_opts, we need this because 
     * of the call of get_solve_dbgopts */
    F77_CALL(init_get_options)(mws_index);

    SEXP names = getAttrib(options, R_NamesSymbol);
    int i;
    for (i = 0; i < length(options); i++) {
        set_option(CHAR(STRING_ELT(names, i)), VECTOR_ELT(options, i));
    }

    F77_CALL(check_options)();
}

static void set_option(const char *name, SEXP value) {
    int i;
    double x;
    if (!strcmp(name, "mode")) {
        CHECK_LENGTH(name, value);
        i = get_imode(name, CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_mode)(&i);
    } else if (!strcmp(name, "fbstart")) {
        CHECK_LENGTH(name, value);
        i = get_istart(name, CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_start)(&i);
    } else if (!strcmp(name, "maxiter")) {
        CHECK_LENGTH(name, value);
        i = get_non_negative_int(name, value);
        F77_CALL(set_maxit)(&i);
    } else if (!strcmp(name, "maxjacupd")) {
        CHECK_LENGTH(name, value);
        i = get_non_negative_int(name, value);
        F77_CALL(set_maxmat)(&i);
    } else if (!strcmp(name, "rlxspeed")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        if (x >= 1) {
            error("%s should be smaller than 1\n", name);
        }
        F77_CALL(set_rlxspeed)(&x);
    } else if (!strcmp(name, "rlxmin")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        if (x > 1) {
            error("%s should be smaller than or equal to 1\n", name);
        }
        F77_CALL(set_rlxmin)(&x);
    } else if (!strcmp(name, "rlxmax")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        if (x > 1) {
            error("%s should be smaller than or equal to 1\n", name);
        }
        F77_CALL(set_rlxmax)(&x);
    } else if (!strcmp(name, "cstpbk")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        F77_CALL(set_cstpbk)(&x);
    } else if (!strcmp(name, "cnmtrx")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        F77_CALL(set_cnmtrx)(&x);
    } else if (!strcmp(name, "xrelax")) {
        CHECK_LENGTH(name, value);
        x = get_positive_number(name, value);
        F77_CALL(set_xrelax)(&x);
    } else if (!strcmp(name, "xmaxiter")) {
        CHECK_LENGTH(name, value);
        i = get_non_negative_int(name, value);
        F77_CALL(set_mratex)(&i);
    } else if (!strcmp(name, "xupdate")) {
        CHECK_LENGTH(name, value);
        i = get_uplead(name, CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_uplead)(&i);
    } else if (!strcmp(name, "dbgopt")) {
        set_debug_opts(value);
    } else if (!strcmp(name, "erropt")) {
        CHECK_LENGTH(name, value);
        i = get_erropt(name, CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_erropt)(&i);
    } else if (!strcmp(name, "report")) {
        CHECK_LENGTH(name, value);
        i = get_repopt(name,  CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_repopt)(&i);
    } else if (!strcmp(name, "ratreport")) {
        CHECK_LENGTH(name, value);
        i = get_ratrepopt(name, CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_ratrepopt)(&i);
    } else if (!strcmp(name, "ratreport_rep")) {
        CHECK_LENGTH(name, value);
        i = get_non_negative_int(name, value);
        F77_CALL(set_ratreport_rep)(&i);
    } else if (!strcmp(name, "ratfullreport_rep")) {
        CHECK_LENGTH(name, value);
        const char *msg = "%s should be an integer";
        if (!isNumeric(value)) {
            error(msg, name); 
        }
        int i = asInteger(value);
        if (i != NA_INTEGER) {
            if (i < 0) {
                error("%s should be a non-negative integer", name); 
            } 
            if (!isInteger(value)) {
                if (i != asReal(value)) {
                    error(msg, name); 
                }
            }
        }
        F77_CALL(set_ratfullreport_rep)(&i);
    } else if (!strcmp(name, "bktmax")) {
        CHECK_LENGTH(name, value);
        i = get_non_negative_int(name, value);
        F77_CALL(set_bktmax)(&i);
    } else if (!strcmp(name, "xtfac")) {
        CHECK_LENGTH(name, value);
        x = get_finite_number(name, value);
        if (x < 2.0) {
            x = 2.0;
            warning("The minimum value of xtfac is 2\n");
        }
        F77_CALL(set_xtfac)(&x);
    } else {
       error("Unknown solve option %s\n", name);
    }
}

static void set_debug_opts(SEXP option) {
    int priter, prexen, jacprt, suptst, xsuptt, prscal;

    F77_CALL(get_solve_dbgopts)(&priter, &prexen, &jacprt, &suptst, &xsuptt, 
                                &prscal);

    int i;
    for (i = 0; i < length(option); i++) {
        const char *opt = CHAR(STRING_ELT(option, i));
        if (!strcmp(opt, DBG_NONE)) {
            priter = 0; prexen = 0; jacprt = 0; suptst = 1; xsuptt = 1;
            prscal = 0;
        } else if (!strcmp(opt, DBG_ALL)) {
            priter = 1; prexen = 1; jacprt = 1; suptst = 0; xsuptt = 0;
            prscal = 1;
        } else {
            int positive = strncmp(opt, "no", 2);
            const char *s = positive ? opt : opt + 2;
            if (!strcmp(s, DBG_PRITER_OPTS[1])) {
                priter = positive;
            } else if (!strcmp(s, DBG_PREXEN_OPTS[1])) {
                prexen = positive;
            } else if (!strcmp(s, DBG_JACPRT_OPTS[1])) {
                jacprt = positive;
            } else if (!strcmp(s, DBG_SUPTST_OPTS[1])) {
                suptst = !positive;
            } else if (!strcmp(s, DBG_XSUPTT_OPTS[1])) {
                xsuptt = !positive;
            } else if (!strcmp(s, DBG_PRSCAL_OPTS[1])) {
                prscal = positive;
            } else {
                error("Unknown solve debug option %s\n", opt);
            }
        }
    }
    F77_CALL(set_solve_dbgopts)(&priter, &prexen, &jacprt, &suptst, &xsuptt, 
                                &prscal);
}
