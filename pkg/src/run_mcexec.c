#include <Rinternals.h>
#include <Rdefines.h>
#include "mchdr.h"
#include "mcopt.h"
#include "prepare_compiler.h"
#include "run_mcexec.h"

SEXP run_mcexec(SEXP filename, SEXP outputfile_, SEXP flags, 
                SEXP include_dirs, const Mcopt *mc_options) {

    const char *modelnm = CHAR(STRING_ELT(filename, 0));
    const char *outputfile = CHAR(STRING_ELT(outputfile_, 0));

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
