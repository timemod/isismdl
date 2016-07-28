#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <ctype.h>
#include "process_solve_options.h"

#define CHECK_LENGTH(name, value) if (length(value) > 1) { \
    error("The value for option %s should have length 1.", name); \
}

extern void F77_SUB(init_set_solve_opts)(int *mws_index, int *use_mws);
extern void F77_SUB(set_mode)(char *);
extern void F77_SUB(set_start)(char *);
extern void F77_SUB(set_maxit)(int *);
extern void F77_SUB(set_maxmat)(int *);
extern void F77_SUB(set_rlxspeed)(double *);
extern void F77_SUB(set_rlxmin)(double *);
extern void F77_SUB(set_rlxmax)(double *);
extern void F77_SUB(set_cstpbk)(double *);
extern void F77_SUB(set_cnmtrx)(double *);
extern void F77_SUB(set_xrelax)(double *);
extern void F77_SUB(set_mratex)(int *);

static void process_option(const char *name, SEXP value);
static char get_mode(const char *s);
static char get_start(const char *s);

void process_solve_options(int *mws_index, int *use_mws, SEXP options) {

    SEXP names = getAttrib(options, R_NamesSymbol);

    F77_CALL(init_set_solve_opts)(mws_index, use_mws);

    int i;
    for (i = 0; i < length(options); i++) {
        process_option(CHAR(STRING_ELT(names, i)), VECTOR_ELT(options, i));
    }
}

static void process_option(const char *name, SEXP value) {
    int i;
    char c;
    double x;
    if (!strcmp(name, "mode")) {
        CHECK_LENGTH(name, value);
        c = get_mode(CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_mode)(&c);
    } else if (!strcmp(name, "fbstart")) {
        CHECK_LENGTH(name, value);
        c = get_start(CHAR(STRING_ELT(value, 0)));
        F77_CALL(set_start)(&c);
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
    } else if (!strcmp(name, "xrelax")) {
        CHECK_LENGTH(name, value);
        x = asReal(value);
        F77_CALL(set_xrelax)(&x);
    } else if (!strcmp(name, "xmaxiter")) {
        CHECK_LENGTH(name, value);
        i = asInteger(value);
        F77_CALL(set_mratex)(&i);
    } else {
       error("Unknown solve option %s\n", name);
    }
}

static char get_mode(const char *s) {
    if (strcmp(s, "dynamic") && strcmp(s, "ratex") && strcmp(s, "reschk") &&
         strcmp(s, "backward") && strcmp(s, "static")) {
            error("Illegal solve mode %s\n.", s);
    }
    return  strcmp(s, "ratex") == 0 ? 'X' : toupper(*s);
}

static char get_start(const char *s) {
    if (!strcmp(s, "previous")) {
        return 'P';
    } else if (!strcmp(s, "current")) {
        return 'C';
    } else if (!strcmp(s, "curifok")) {
        return 'Q';
    } else if (!strcmp(s, "previfok")) {
        return 'D';
    } else {
        error("Illegal value for optiopn fbstart %s\n.", s);
    }
}
