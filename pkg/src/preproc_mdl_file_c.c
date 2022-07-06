#include <Rinternals.h>
#include <Rdefines.h>
#include "mchdr.h"
#include "preproc_mdl_file_c.h"
#include "run_mcexec.h"

SEXP preproc_mdl_file_c(SEXP filename, SEXP outputfile, SEXP flags, 
                        SEXP include_dirs) {

    Mcopt mc_options;
    mcopt_init(&mc_options);
    mc_options.McPreproc = 1;
    mc_options.Strict = 1;

    return run_mcexec(filename, outputfile, flags, include_dirs,
                      &mc_options);
}

