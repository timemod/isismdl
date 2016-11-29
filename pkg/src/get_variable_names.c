#include <R.h>
#include <Rinternals.h>

extern int F77_NAME(get_variable_count)(void);
extern void F77_NAME(get_variable_name)(int *x, char* variable_name);
extern void F77_NAME(get_mdl_data)(int *model_index, int *variable_index,
                                   double *mdl_data);

SEXP get_model_variables_(void) {

    char variable_name[5];
    int i, j;
    int variable_count = F77_CALL(get_variable_count)();
    SEXP names = PROTECT(allocVector(STRSXP, variable_count));
    for (i = 0; i < variable_count; i++) {
        j = i + 1;
        F77_CALL(get_variable_name)(&j, variable_name);
        SET_STRING_ELT(names, i, mkChar(variable_name));
    }
    UNPROTECT(1);
    return names;
}
