#include <Rinternals.h>
#include <Rdefines.h>
#include "process_solve_options.h"

void set_solve_opts_c(SEXP mws_index_, SEXP options) {

    // process arguments
    int mws_index = asInteger(mws_index_);
    int opts_present = length(options) > 0;
    int use_mws = 1;
    if (opts_present) {
        process_solve_options(&mws_index, &use_mws, options);
    }
}
