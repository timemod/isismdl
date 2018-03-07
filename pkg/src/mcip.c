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
    const char *dum = NULL;
    return mcexec(mfname, dum, options);
}
