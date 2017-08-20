#include <Rinternals.h>
#include <Rdefines.h>
#include "flags.h"
#include "include_paths.h"
#include "prepare_compiler.h"

void prepare_compiler(SEXP flags, SEXP include_dirs) {

    int i;

    /* compiler flags */
    clear_flags();
    if (!Rf_isNull(flags)) {
        for (i = 0; i < length(flags); i++) {
            const char *flag = CHAR(STRING_ELT(flags, i));
            add_flag(flag);
        }
    }

    /* include directories */
    init_include_dirs();
    if (!Rf_isNull(include_dirs)) {
        for (i = 0; i < length(include_dirs); i++) {
            const char *dir = CHAR(STRING_ELT(include_dirs, i));
            add_include_path(dir);
        }
    }
}


