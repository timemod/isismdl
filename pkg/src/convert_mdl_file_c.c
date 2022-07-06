#include <Rinternals.h>
#include <Rdefines.h>
#include "mchdr.h"
#include "convert_mdl_file_c.h"
#include "run_mcexec.h"

static SEXP getListElement(SEXP list, const char *str);

SEXP convert_mdl_file_c(SEXP filename, SEXP outputfile, SEXP flags, 
                        SEXP include_dirs, SEXP options) {

    Mcopt mc_options;
    mcopt_init(&mc_options);
    mc_options.Strict = 1; 

    SEXP subst_ = getListElement(options, "substitute");
    if (!Rf_isNull(subst_)) {
       mc_options.Substufunc = asInteger(subst_);
    }

    /* there are currently two possible ouput:
     * isismdl syntax (Showocode == 1) or dynare models (MakeDynare == 1)
     */
    SEXP mk_dyn_ = getListElement(options, "make_dynare");
    if (!Rf_isNull(mk_dyn_)) {
       mc_options.MakeDynare = asInteger(mk_dyn_);
    } 
    mc_options.Showocode = mc_options.MakeDynare == 0;

    return run_mcexec(filename, outputfile, flags, include_dirs,
		      &mc_options);
 }

static SEXP getListElement(SEXP list, const char *str) {
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;
    for (i = 0; i < length(list); i++)
        if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
            elmt = VECTOR_ELT(list, i);
            break;
        }
    return elmt;
}
