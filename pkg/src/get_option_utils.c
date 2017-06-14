#include <Rinternals.h>
#include <Rdefines.h>
#include "get_option_utils.h"

static SEXP options,names;
static int cnt;

void init_options(int n_options) {
    options = PROTECT(allocVector(VECSXP, n_options));
    names = PROTECT(allocVector(STRSXP, n_options));
    cnt = 0;
}

void add_option(const char *name, SEXP value) {
    SET_STRING_ELT(names, cnt, mkChar(name));
    SET_VECTOR_ELT(options, cnt++, value);
}

SEXP get_options(void) {
    setAttrib(options, R_NamesSymbol, names);
    UNPROTECT(2);
    return options;
}


