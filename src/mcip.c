/*
 * Wrapper routines called from Isis 
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mchdr.h"
#include "flags.h"
#include "util.h"

int mcip(char *mfname, int *strict, int *gen_dep_file) {
    Mcopt options;
    mcopt_init(&options);
    options.Strict = *strict;
    options.gen_dep = *gen_dep_file;
    return mcexec(mfname, options);
}

void add_include_dir(char *dirname) {
    /*
     * Called from Fortran code: add directory to the name of
     * include files */

    add_include_path((char *) dirname);
}

void init_incl_dirs(void) {
    init_include_dirs();
}
