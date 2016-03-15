/*
 * Wrapper routines called from Isis 
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "macromodtypes.h"
#include "mchdr.h"
#include "flags.h"
#include "util.h"

FINT FNAME(mcip)(FUCHAR *mfname, FINT *strict, FINT *gen_dep_file) {
    Mcopt options;
    mcopt_init(&options);
    options.Strict = *strict;
    options.gen_dep = *gen_dep_file;
    return mcexec((char *) mfname, options);
}

void FNAME(add_include_dir)(FUCHAR *dirname)
{
    /*
     * Called from Fortran code: add directory to the name of
     * include files */

    add_include_path((char *) dirname);
}

void FNAME(add_flag)(FUCHAR *flag, FINT *error) 
{
    char *fl = estrdup((char *) flag); 
    *error = add_flag(fl);
}

void FNAME(init_incl_dirs)(void)
{
    init_include_dirs();
}
