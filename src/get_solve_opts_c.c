#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#define N_OPTS 3

extern void F77_SUB(init_get_solve_opts)(int *mws_index);
extern char F77_SUB(get_mode)(void);
extern char F77_SUB(get_start)(void);
extern int  F77_SUB(get_maxit)(void);

char *get_mode_text(char *buf);
char *get_start_text(char *buf);
static void add_option(const char *name, SEXP value);

static SEXP ret, names;
static int cnt;

SEXP get_solve_opts_c(SEXP mws_index_) {

    int mws_index = asInteger(mws_index_);

    F77_CALL(init_get_solve_opts)(&mws_index);

    ret  = PROTECT(allocVector(VECSXP, N_OPTS));
    names = PROTECT(allocVector(STRSXP, N_OPTS));

    cnt = 0;
    char buf[100];

    add_option("mode", PROTECT(mkString(get_mode_text(buf))));
    add_option("fbstart", PROTECT(mkString(get_start_text(buf))));
    add_option("maxiter", PROTECT(ScalarInteger(F77_CALL(get_maxit)())));

    setAttrib(ret, R_NamesSymbol, names);

    UNPROTECT(2 + N_OPTS);
    return ret;
}

static void add_option(const char *name, SEXP value) {
    SET_STRING_ELT(names, cnt, mkChar(name));
    SET_VECTOR_ELT(ret, cnt++, value);
}

char *get_mode_text(char *buf) {
    char c = F77_CALL(get_mode)();
    switch (c) {
    case 'D': 
        strcpy(buf, "dynamic"); break;
    case 'X': 
        strcpy(buf, "ratex"); break;
    case 'R': 
        strcpy(buf, "reschk"); break;
    case 'B': 
        strcpy(buf, "backward"); break;
    case 'S': 
        strcpy(buf, "static"); break;
    default:
       strcpy(buf, "???");
    };
    return buf;
}

char * get_start_text(char *buf) {
    char c = F77_CALL(get_start)();
    switch (c) {
    case 'P': 
        strcpy(buf, "previous"); break;
    case 'C': 
        strcpy(buf, "current"); break;
    case 'Q': 
        strcpy(buf, "curifok"); break;
    case 'D': 
        strcpy(buf, "previfok"); break;
    default:
        strcpy(buf,"???");
    };
    return buf;
}
