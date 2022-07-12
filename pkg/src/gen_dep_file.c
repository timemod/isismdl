#include <Rinternals.h>
#include <Rdefines.h>
#include "mchdr.h"
#include "gen_dep_file.h"
#include "run_mcexec.h"

/* Generate a file with dependency informatie.
 * Filename should be the name of mdl file without user functions
 * and without preprocessor directives */

SEXP gen_dep_file(SEXP filename, SEXP outputfile) {

    Mcopt mc_options;
    mcopt_init(&mc_options);
    mc_options.Strict = 1;
    mc_options.gen_dep = 1;

    return run_mcexec(filename, outputfile, R_NilValue, R_NilValue,
                      &mc_options);
}

