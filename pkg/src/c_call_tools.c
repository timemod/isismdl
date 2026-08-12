/* 
 * Helper functions for calling Fortran code from C
 */

#include <Rinternals.h>
#include <Rdefines.h>
#include "c_call_tools.h"
#include "isismdl_types.h"
#include "get_info.h"

/* Return an unprotected SEXP with variable names from an integer array of variable 
 * indices.*/
SEXP get_variable_names(int *indices, int n, int model_index) {
        
    SEXP names = PROTECT(allocVector(STRSXP, n));
    
    char name[MCMXNM + 1]; /* +1 because of terminating '\0' */
    int alpha = 0;
    int i, len, index;
    
    for (i = 0; i < n; i++) {
        index = indices[i]; 
        F77_CALL(get_variable_name)(&model_index, &index, name, &len, &alpha);
        name[len] = '\0';
        SET_STRING_ELT(names, i, mkChar(name));
    }

    UNPROTECT(1);
    
    return names;
}   

