/*
 * do something with .tsp/.mdl model definition file
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "symbol.h"
#include "xpcdef.h"
#include "util.h"
#include "mcinit.h"
#include "fname.h"
#include "ecode.h"

#include "mchdr.h"
#include "flags.h"

#include "outecode.h"
#include "outmdl.h"
#include "futils.h"

#include "dependencies.h"
#include "isismdl.h"
#include "mdldef.h"

#define CPUSECS(t0,t1) (((double)(t1)-(double)(t0))/(double)(CLOCKS_PER_SEC))

/* return codes of mcexec: */
#define FILE_ERROR    1
#define SYNTAX_ERROR  2

/*
 * global variables
 */

int scanning;  /* 1 if scanning model for parameters, 0 otherwise */
Mcopt options; /* compilation options */

/*
 * for filename handling
 */

char    path [FILENAME_MAX + 1];
char    base [FILENAME_MAX + 1];
char    ext  [FILENAME_MAX + 1];
char    xtra [FILENAME_MAX + 1];

char    *mkfname(char *fname, char *path, char *base, char *ext);

static void do_dep(FILE *);               /* for dependency output   */
static void do_dep_free(void);            /* free dependency trees   */

static void do_zrf(FILE *);               /* for zrf output   */


/*
 * warnings and errors
 */

static  void vmessage(const char *fmt , va_list args);

char    msgname[FILENAME_MAX + 1];      /* warning message filename     */
static  FILE *ferr = NULL;              /* warning output stream        */

size_t  warncnt    = 0;                 /* # of warnings written        */
size_t   errcnt    = 0;                 /* # of errors   written        */


static void reset(void) {

    if (ferr != NULL) {
        fclose(ferr);
        ferr = NULL;
    }
    warncnt = 0;
    errcnt = 0; 

    if (options.gen_dep) do_dep_free();

    free_symtab(Stp);
    Stp = NULL;
    free_symtab(Eqntp);
    Eqntp = NULL;
    free_enodes();
    if (options.McIsisMdl) {
        free_polish();
    }
    clear_flags();
}


/* Call the model parsers.
 * mfname: name of the mdl file
 * outputfile: if this function is called by mcisis, outputfile is the name
 *             of the file to which the preprocessed mdl file is written.
 *             if this function is called from convert_mdl_c, outputfile is the
 *             name of the ouptu file with converted mdl.
 */
int mcexec(const char *mfname, const char *outputfile, const Mcopt *options_in) {
    FILE    *mp;
    char    *fnmdl;
    char    fname[FILENAME_MAX + 1];

    clock_t tb = clock(), te;
    double  cpusecs;

    options = *options_in;

    /* check output options */
    int output_cnt = options.McPreproc + options.Showecode + 
	             options.Showocode + options.MakeTroll + 
		     options.MakeEviews + options.MakeDynare +
		     options.gen_dep;
    if (output_cnt > 1) {
        ERROR("Only a single output type can be selected");
    } else if ((output_cnt != 0 && !(output_cnt == 1 && options.McPreproc))
		    && options.McIsisMdl) {
        ERROR("Options McIsisMdl is incompatible with output options");
    } else if (output_cnt == 1 && outputfile == NULL) {
        ERROR("Argument outputfile should not be NULL");
    }

    /*
     * declares and initializes Stp, the symbol table
     */

    mcinit();
    if (output_cnt > 0) {
        mdldef_init();
    }

    strcpy(fname, mfname);

    fnsplit(fname, path, base, ext);
    
    /* if the input filename fname does not have an extension, 
     * then add extension "mdl" */
    fnmdl = (*ext == 0 ) ? mkfname(fname, path, base, "mdl") : fname;

    /* create error file */
    mkfname(msgname, path, base, "err" );   /* for errors and warnings */

    /*
     * delete message file if present
     */

    mp = fopen( msgname, "r" );
    if( mp )
    {
        fclose( mp );
        remove( msgname );
    }

    /*
     * execute the parse
     */

#if 0
    yydebug = options.Debug;
#endif


    if (!file_exists(fnmdl)) {
        if (!options.McIsisMdl) {
            ERROR("File %s does not exist\n", fnmdl);
        }
        return FILE_ERROR;
    }


    if (!options.Strict) {
         /* non-strict compilation: parameters may be used before
          * they have been specified. Therefore first scan model
          * for parameters
          */

        if (options.ShowTiming) {
            Rprintf("Scanning model for parameters ...\n" );
            tb = clock();
        }
        init_scanner(fnmdl, NULL);
        scanning = 1;
        mc_scan_params();
        scanning = 0;

        mcrestart(NULL);

        if (options.ShowTiming) {
            te = clock();
            cpusecs = CPUSECS(tb,te);
            Rprintf("Scanning used %.2f seconds\n", cpusecs);

            Rprintf("Parsing model ...\n" );
            tb = clock();
        }
    }

    if (options.ShowTiming) {
       tb = clock();
    }

    if (options.McPreproc) {
      init_scanner(fnmdl, outputfile);
    } else {
      init_scanner(fnmdl, NULL);
    }
 
    /* initialise parser (needed when mcexec is called more than once)
     */
    mcparse_init();

    mcparse();

    if (options.ShowTiming) {
        te = clock();
        cpusecs = CPUSECS(tb,te);
        Rprintf("Parse used %.2f seconds\n", cpusecs);
    }

    if (warncnt || errcnt) {
        if (!options.McIsisMdl) {
            ERROR("Warnings and/or errors written to .err file\n");
        }
        reset();
        return SYNTAX_ERROR;
    }

    /*
     * don't come here if warnings or errors detected
     */

    /*
     * output a legible listing of internal binary code
     */

    if (options.Showecode) {
        FILE *feco = efopen(outputfile, "w" );
        out_ecode(feco);
        fclose(feco);
    }

    /*
     * output model code in some form
     */

    if (options.Showocode) {
        FILE *foco = efopen(outputfile,  "w" );
        out_omdl(foco, options.Substufunc);
        fclose(foco);
    }

    /*

    if (options.MakeTroll) {
    
        ERROR("It is not yet possible to generate Troll output");


        mkfname(outputfile, path, base, "inp" ); 
        foco = efopen(outputfile, "w" );

        strcpy(xtra, base);
        strcat(xtra, "param");

        mkfname( xtrname, path, xtra, "inp" ); 
        fxtr = efopen( xtrname, "w" );

        out_tmdl(foco, fxtr);
        fclose(foco);
    }
    */

    if (options.MakeDynare) {
        FILE *foco = efopen(outputfile, "w" );
        out_dmdl(foco, options.Substufunc);
        fclose(foco);
    }

    /*
    if( options.MakeEviews) {
        FILE *foco = efopen(outputfile, "w" );
        out_vmdl(foco, options.mdlname);
        fclose(foco);
    }
    */

    if (options.gen_dep) {
        FILE *fdep = efopen(outputfile, "w" );
        do_dep(fdep);
        fclose(fdep);
    }

    if (options.McIsisMdl) {
        /* export the symbol table to the fortran subroutines */
        export_symtab();
    } else {
         if( options.Makezrf) {
            /*
             * write cross reference
             */
             char zrfname[FILENAME_MAX + 1];
             mkfname(zrfname, path, base, "zrf" );
             FILE *fzrf = efopen(zrfname, "w");
             do_zrf(fzrf);
             fclose(fzrf);
         }
    
        if( options.Showhash ) {
            PRINTF("Statistics for Equation table\n");
            sym_stat(Eqntp);
    
            PRINTF("Statistics for Symbol table\n");
            sym_stat(Stp);
        }
    }
    
    reset();
    return 0;
    }
    
char *mkfname( char *fname, char *path, char *base, char *ext) {
    char *fn;
    
    fn = fnjoin( fname, path, base, ext);
    if (fn == NULL) {
        ERROR( "Bad filename\n");
    }
    return fn;
}

static void vmessage(const char *fmt , va_list args) {

    if (ferr == NULL) {
        ferr = fopen( msgname, "w" );
        if (ferr == NULL) {
            ERROR("Unable to open the error file\n");
        }
    }

    vfprintf(ferr, fmt, args);
}

void mcerrmessage(const char *fmt , va_list args) {
        ++errcnt;

        vmessage(fmt, args);
        fputc('\n', ferr);
        showbuf( ferr );
        fputc('\n', ferr);
}


int mcerror(const char *fmt , ... ) {
    /*
     * called by mcparse and mclex
     */

    va_list args;

    va_start(args, fmt);
    mcerrmessage(fmt, args);
    va_end(args);

    return 0;
}

void mcwarn(const char *fmt , ... ) {
    va_list args;

    ++warncnt;
    va_start(args, fmt);
    vmessage(fmt, args);
    va_end(args);
}

static FILE *fzzout = NULL;

/* dep_out: output dependency information for a single equation */
static int dep_out(Symbol *sp ) {
    if (sp->xpctype == XP_EQN) {
        Equation *eq = sp->u.eqnp;
        if (eq->deps != NULL) {
            print_dependencies(fzzout, eq->lhs->name, eq->deps);
        } 
    }
    return NXTSYM;
}


/* do_dep: output dependency information for all equations in the symbol table */
static void do_dep(FILE *fzout) {
    fzzout = fzout;
    sym_walk(Eqntp, dep_out, NULL );
}

/*
 * cross reference
 */

static int var_out(Symbol *sp) {
    if (sp->xpctype == XP_ENDO || sp->xpctype == XP_EXO) {
        Variable *vp = sp->u.varp;
        if (vp) {
            fprintf(fzzout, " %-12s %4d %4d\n", sp->name,
                    vp->maxlag, vp->maxlead);
        } else {
            fprintf(fzzout, "%s vp == NULL?\n", sp->name);
        }
    }
    return NXTSYM;
}

static void do_zrf (FILE *fzout) {
    fzzout = fzout;
    sym_walk(Stp, var_out, NULL );
}

/* dep_free: deallocate memory of dependency structure */
static int dep_free(Symbol *sp ) {
    if (sp->xpctype == XP_EQN) {
        Equation *eq = sp->u.eqnp;
        if (eq->deps != NULL) {
            free_dependencies(eq->deps);
        } 
    }
    return NXTSYM;
}

/* do_dep_free: free dependency trees for all equations */
static void do_dep_free(void) {
    sym_walk(Eqntp, dep_free, NULL);
}
