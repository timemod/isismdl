#include <Rinternals.h>
#include <Rdefines.h>
#include "prepare_compiler.h"
#include "compile_mdl_c.h"

extern void F77_NAME(mcisis)(int *modelnmlen, const char *modelnm,
                             int *mifnmlen, const char *mifnm,
                             int *ppfnmlen, const char *ppfnm,
                             int *idofbrd, int *igenfbo, int *ifbomif,
                             int *iprifb, int *iprisjc, int *mrfopt,
                             int *fbcopt, int *mcstat);

SEXP compile_mdl_c(SEXP filename, SEXP mifname, SEXP ppfname,
		   SEXP flags, SEXP include_dirs) {

    const char *modelnm = CHAR(STRING_ELT(filename, 0));
    int modelnmlen = strlen(modelnm);
    const char *mifnm = CHAR(STRING_ELT(mifname, 0));
    int mifnmlen = strlen(mifnm);
    const char *ppfnm = CHAR(STRING_ELT(ppfname, 0));
    int ppfnmlen = strlen(ppfnm);

    /* initialise options */
    int idofbrd, igenfbo, ifbomif, iprifbi, iprisjc,
        mrfopt[2], fbcopt[2];

    idofbrd = 1;
    igenfbo = 1;
    ifbomif = 0;
    iprifbi = 0;
    iprisjc = 0;
    fbcopt[0] = 0;
    fbcopt[1] = 200;
    mrfopt[0] = 128;
    mrfopt[1] = 1;

    prepare_compiler(flags, include_dirs);

    int mcstat;
    F77_CALL(mcisis)(&modelnmlen, modelnm, &mifnmlen, mifnm, 
		     &ppfnmlen, ppfnm, 
		     &idofbrd, &igenfbo, &ifbomif,
                     &iprifbi, &iprisjc, mrfopt, fbcopt, &mcstat);

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
