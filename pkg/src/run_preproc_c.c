#include <Rinternals.h>
#include <Rdefines.h>
#include "mchdr.h"
#include "run_preproc_c.h"
#include "run_mcexec.h"

/* Run the model preprocessor to get a fully preprocessed model */
SEXP run_preproc_c(SEXP filename, SEXP outputfile, SEXP flags, 
                        SEXP include_dirs) {

    Mcopt mc_options;
    mcopt_init(&mc_options);
    mc_options.McPreproc = 1;
    mc_options.Strict = 1;

    return run_mcexec(filename, outputfile, flags, include_dirs,
                      &mc_options);
}

