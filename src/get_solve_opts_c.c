#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#define N_OPTS 3

extern void F77_SUB(init_get_solve_opts)(int *mws_index);
extern void F77_SUB(get_solve_options)(int *, int *, int *);

static char *get_mode_text(char *buf, int mode);
static char *get_start_text(char *buf, int start);
static void add_option(const char *name, SEXP value);

static SEXP ret, names;
static int cnt;
static char buf[20];

SEXP get_solve_opts_c(SEXP mws_index_) {

    int mws_index = asInteger(mws_index_);

    F77_CALL(init_get_solve_opts)(&mws_index);

    ret  = PROTECT(allocVector(VECSXP, N_OPTS));
    names = PROTECT(allocVector(STRSXP, N_OPTS));

    cnt = 0;

    int imode, istart, maxit;
    F77_CALL(get_solve_options)(&imode, &istart, &maxit);

    add_option("mode", PROTECT(mkString(get_mode_text(buf, imode))));
    add_option("fbstart", PROTECT(mkString(get_start_text(buf, istart))));
    add_option("maxiter", PROTECT(ScalarInteger(maxit)));

    setAttrib(ret, R_NamesSymbol, names);

    UNPROTECT(2 + N_OPTS);
    return ret;
}

static void add_option(const char *name, SEXP value) {
    SET_STRING_ELT(names, cnt, mkChar(name));
    SET_VECTOR_ELT(ret, cnt++, value);
}

static char *get_mode_text(char *buf, int imode) {
    switch (imode) {
    case 1: 
        strcpy(buf, "dynamic"); break;
    case 2: 
        strcpy(buf, "ratex"); break;
    case 3: 
        strcpy(buf, "reschk"); break;
    case 4: 
        strcpy(buf, "backward"); break;
    case 5: 
        strcpy(buf, "static"); break;
    default:
       strcpy(buf, "???");
    };
    return buf;
}

static char * get_start_text(char *buf, int istart) {
    switch (istart) {
    case 1: 
        strcpy(buf, "previous"); break;
    case 2: 
        strcpy(buf, "current"); break;
    case 3: 
        strcpy(buf, "curifok"); break;
    case 4: 
        strcpy(buf, "previfok"); break;
    default:
        strcpy(buf,"???");
    };
    return buf;
}
