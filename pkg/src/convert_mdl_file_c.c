#include <Rinternals.h>
#include <Rdefines.h>
#include "prepare_compiler.h"
#include "mchdr.h"

static SEXP getListElement(SEXP list, const char *str);

SEXP convert_mdl_file_c(SEXP filename, SEXP outputfile_, SEXP flags, 
                        SEXP include_dirs, SEXP options) {

    const char *modelnm = CHAR(STRING_ELT(filename, 0));
    const char *outputfile = CHAR(STRING_ELT(outputfile_, 0));

    Mcopt mc_options;
    mcopt_init(&mc_options);
    mc_options.McIsisMdl = 0;
    mc_options.Strict = 1;
    mc_options.Showocode = 1;
    SEXP subst_ = getListElement(options, "substitute");
    if (!Rf_isNull(subst_)) {
       mc_options.Substufunc = asInteger(subst_);
    }
    SEXP mk_dyn_ = getListElement(options, "make_dynare");
    if (!Rf_isNull(mk_dyn_)) {
       mc_options.MakeDynare = asInteger(subst_);
    }

    prepare_compiler(flags, include_dirs);

    int mcstat =  mcexec(modelnm, outputfile, mc_options);

    if (mcstat != 0) {
        char *format;
        switch (mcstat) {
            case 1: format = "The model file %s does not exist\n";
                    break;
            case 2: format = "Error detected in compilation of model %s\nCheck the .err file";
                    break;
            case 4: format = "Not enough memory to compile the model %s\n";
                    break;
            default: format = "Error detected in compilation of model %s\n";
                     break;
        }
        error(format, modelnm);
    }
    

    return ScalarLogical(mcstat == 0);
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


