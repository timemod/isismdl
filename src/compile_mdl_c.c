#include <Rinternals.h>
#include <Rdefines.h>

extern void F77_NAME(mcisis)(int *modelnmlen, const char *modelnm,
                             int *idofbrd, int *igenfbo, int *ifbomif,
                             int *iprifb, int *iprisjc, int *mrfopt,
                             int *fbcopt, int *igen_dep_file, int *mcstat);

SEXP compile_mdl_c(SEXP filename) {

    const char *modelnm = CHAR(STRING_ELT(filename, 0));
    int modelnmlen = strlen(modelnm);
    int idofbrd, igenfbo, ifbomif, iprifbi, iprisjc,
        mrfopt[2], fbcopt[2], igen_dep_file, mcstat;

    /* initialise option */
    idofbrd = 1;
    igenfbo = 1;
    ifbomif = 0;
    iprifbi = 0;
    iprisjc = 0;
    fbcopt[0] = 0;
    fbcopt[1] = 200;
    mrfopt[0] = 128;
    mrfopt[1] = 1;

    F77_CALL(mcisis)(&modelnmlen, modelnm, &idofbrd, &igenfbo, &ifbomif,
                     &iprifbi, &iprisjc, mrfopt, fbcopt, &igen_dep_file, 
                     &mcstat);
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
    return R_NilValue;
}
