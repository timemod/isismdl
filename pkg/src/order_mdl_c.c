#include <Rinternals.h>
#include <Rdefines.h>

extern void F77_NAME(msordr)(int *model_index, int *genfbo_specified,
                             int *genfbo);

SEXP order_mdl_c(SEXP model_index_, SEXP orfnam) {

    int model_index = asInteger(model_index_);
    //const char *modelnm = CHAR(STRING_ELT(filename, 0));
    //int modelnmlen = strlen(modelnm);


    /* initialise options */
    int genfbo_specified = 0, genfbo = 0;

    F77_CALL(msordr)(&model_index, &genfbo_specified, &genfbo);

    return R_NilValue;
}   
