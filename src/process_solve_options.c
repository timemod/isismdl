#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include <ctype.h>

extern void F77_SUB(init_set_solve_opts)(int *mws_index);
extern void F77_SUB(set_maxit)(int *maxit);
extern void F77_SUB(set_mode)(char *mode);
void process_option(const char *name, SEXP value);

void process_solve_options(int *mws_index, SEXP options) {

    SEXP names = getAttrib(options, R_NamesSymbol);

    F77_CALL(init_set_solve_opts)(mws_index);

    int i;
    for (i = 0; i < length(options); i++) {
        process_option(CHAR(STRING_ELT(names, i)), VECTOR_ELT(options, i));
    }
}

void process_option(const char *name, SEXP value) {
    int i;
    char c;
    if (strcmp(name, "mode") == 0) {
        /* todo: error in length(SEXP) > 1? */
        /* todo: check allowed mode values, error if incorrect mode ,
         * use a function get_mode or something */
        const char *s = CHAR(STRING_ELT(value, 0));
        c = strcmp(s, "ratex") == 0 ? 'X' : toupper(*s);
        F77_CALL(set_mode)(&c);
    } else if (strcmp(name, "maxiter") == 0) {
        i = asInteger(value);
        F77_CALL(set_maxit)(&i);
    } else {
       error("Unknown solve option %s\n", name);
    }
}
