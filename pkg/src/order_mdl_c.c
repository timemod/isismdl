#include <Rinternals.h>
#include <Rdefines.h>

extern void F77_NAME(msordr)(int *model_index, int *genfbo_specified,
                             int *genfbo, int *orfnmlen, const char *orfnm);

SEXP order_mdl_c(SEXP model_index_, SEXP orfnam) {

    int model_index = asInteger(model_index_);

    const char *orfnm = (Rf_isNull(orfnam)) ? "" : CHAR(STRING_ELT(orfnam, 0));
    int orfnmlen = strlen(orfnm);

    /* initialise options */
    int genfbo_specified = 0, genfbo = 0;

    F77_CALL(msordr)(&model_index, &genfbo_specified, &genfbo, &orfnmlen, 
                     orfnm);

    return R_NilValue;
}   
