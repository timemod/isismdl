#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mchdr.h"
#include "flags.h"
#include "util.h"

/* mfname: name of the model file (possibly with extention .mdl)
 * ppfname: name of the preprocessor file that will be created */
int mcip(const char *mfname, const char *ppfname,
         int *strict, int *gen_dep_file) {
    Mcopt options;
    mcopt_init(&options);
    options.McIsisMdl = 1;
    options.McPreproc = 1;
    options.Strict = *strict;
    options.gen_dep = *gen_dep_file;
    return mcexec(mfname, ppfname, &options);
}
