#include <Rinternals.h>
#include <Rdefines.h>
#include "prepare_compiler.h"
#include "compile_mdl_c.h"

extern void F77_NAME(mcisis)(int *modelnmlen, const char *modelnm,
                             int *mifnmlen,   const char *mifnm,
                             int *mrfnmlen,   const char *mrfnm,
                             int *ppfnmlen,   const char *ppfnm);

 SEXP compile_mdl_c(SEXP filename, SEXP mifname, SEXP mrfname,
                    SEXP ppfname, SEXP flags, SEXP include_dirs) {

    const char *modelnm = CHAR(STRING_ELT(filename, 0));
    int modelnmlen = strlen(modelnm);
    const char *mifnm = CHAR(STRING_ELT(mifname, 0));
    int mifnmlen = strlen(mifnm);
    const char *mrfnm = CHAR(STRING_ELT(mrfname, 0));
    int mrfnmlen = strlen(mrfnm);
    const char *ppfnm = CHAR(STRING_ELT(ppfname, 0));
    int ppfnmlen = strlen(ppfnm);

    prepare_compiler(flags, include_dirs);

    F77_CALL(mcisis)(&modelnmlen, modelnm, &mifnmlen, mifnm,
                     &mrfnmlen, mrfnm, &ppfnmlen, ppfnm);

    return R_NilValue;
}
