#include <Rinternals.h>
#include <Rdefines.h>
#include "set_fit_opts_c.h"
#include "init_set_get_options.h"
#include "set_fit_options.h"

/* Sets the solve options. Directly called from R */
SEXP set_fit_opts_c(SEXP model_index_, SEXP options) {
    int model_index = asInteger(model_index_);
    int opts_present = length(options) > 0;
    if (opts_present) {
        int use_mws = 1;
        F77_CALL(init_set_options)(&model_index, &use_mws);
        set_fit_options(&model_index, options);
    }
    return R_NilValue;
}
