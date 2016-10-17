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

#ifndef MCISIS
#include "outecode.h"
#endif
#include "outmdl.h"
/* futils.h is located in libc */
#include "futils.h"

#include "dependencies.h"
#include "isismdl.h"

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
#ifndef MCISIS
char    xtra [FILENAME_MAX + 1];
#endif

char    *mkfname(char *fname, char *path, char *base, char *ext);

FILE    *feco = NULL;                   /* for ecode output */
char    econame[FILENAME_MAX + 1];

static void do_dep(FILE *);               /* for dependency output   */

#ifndef MCISIS
FILE    *foco = NULL;                   /* for model output */
char    oconame[FILENAME_MAX + 1];

FILE    *fxtr = NULL;                   /* additional file */
char    xtrname[FILENAME_MAX + 1];

static void do_zrf(FILE *);               /* for zrf output   */

#endif

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

    free_symtab(Stp);
    Stp = NULL;
    free_symtab(Eqntp);
    Eqntp = NULL;
    free_enodes();
#ifdef MCISIS
    free_polish();
#endif
    clear_flags();
}

int mcexec(char *mfname, Mcopt options_in) {
    FILE    *mp;
    char    *fnmdl;
    char    fname[FILENAME_MAX + 1];

#ifndef MCISIS
    clock_t tb, te;
    double  cpusecs;
#endif

    options = options_in;

    /*
     * declares and initializes Stp, the symbol table
     */

    mcinit();

    strcpy(fname, mfname);

    fnsplit(fname, path, base, ext);
    
    /* if the input filename fname does not have an extension, 
     * then add extension "mdl" */
    fnmdl = (*ext == 0 ) ? mkfname(fname, path, base, "mdl") : fname;

    /* if argument outputname has been specified, then determine the
     * path and base of the outputname and use these variables 
     * to generate the output filenames */
    if (options.outputname > 0) {
        fnsplit(options.outputname, path, base, ext);
    }


#ifndef MCISIS
    mkfname( econame, path, base, "eco" );   /* for ecode           */
    mkfname( oconame, path, base, "oco" );   /* for inorder output  */
#endif
    mkfname( msgname, path, base, "err" );   /* for errors and warnings */

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
#ifndef MCISIS
        fprintf( stderr, "File %s does not exist\n", fnmdl);
#endif
        return FILE_ERROR;
    }

    init_scanner(fnmdl);

    if (!options.Strict) 
         /* non-strict compilation: parameters may be used before
          * they have been specified. Therefore first scan model
          * for parameters
          */
    {

#ifndef MCISIS
        fprintf( stderr, "Scanning model for parameters ...\n" );
        tb = clock();
#endif
        scanning = 1;
        mc_scan_params();
        scanning = 0;

        init_scanner(fnmdl);
        mcrestart(NULL);

#ifndef     MCISIS
        te = clock();
        cpusecs = CPUSECS(tb,te);
        fprintf(stderr,"Scaning used %.2f seconds\n", cpusecs);

        fprintf( stderr, "Parsing model ...\n" );
        tb = clock();
#endif
    }


    /* initialise parser (needed when mcexec is called more than once)
     */
    mcparse_init();

    mcparse();

#ifndef MCISIS
    te = clock();
    cpusecs = CPUSECS(tb,te);
    fprintf(stderr,"Parse used %.2f seconds\n", cpusecs);
#endif

    if( warncnt || errcnt ) {
#ifndef MCISIS
        fprintf(stderr, "Warnings and/or errors written to .err file\n");
#endif
        reset();
        return SYNTAX_ERROR;
    }

    /*
     * don't come here if warnings or errors detected
     */

    /*
     * output a legible listing of internal binary code
     */

#ifndef MCISIS

    if( options.Showecode )
    {
        feco = efopen( econame, "w" );
        out_ecode(feco);
        fclose(feco);
    }

    /*
     * output model code in some form
     */

    if( options.Showocode )
    {
        foco = efopen( oconame, "w" );
        out_omdl(foco, options.Substufunc);
        fclose(foco);
    }

    if( options.MakeTroll )
    {
        /*
         * reuse oconame
         */

        mkfname( oconame, path, base, "inp" ); 
        foco = efopen( oconame, "w" );

        strcpy(xtra, base);
        strcat(xtra, "param");

        mkfname( xtrname, path, xtra, "inp" ); 
        fxtr = efopen( xtrname, "w" );

        out_tmdl(foco, fxtr);
        fclose(foco);
    }

    if( options.MakeEviews )
    {
        /*
         * reuse oconame
         */

        mkfname( oconame, path, base, "prg" ); 
        foco = efopen( oconame, "w" );

        out_vmdl(foco, options.mdlname);
        fclose(foco);
    }
#endif

if (options.gen_dep) {
    /* reuse variables econame and feco */
    mkfname(econame, path, base, "dep");   
    feco = efopen(econame, "w" );
    do_dep(feco);
    fclose(feco);
}

    
#ifdef MCISIS
    /* export the symbol table to the fortran subroutines */
    export_symtab();
#else
    /*
     * write cross reference
     *
     * reuse econame
     */

     mkfname( econame, path, base, "zrf" );   
     feco = efopen( econame, "w" );
     do_zrf( feco );
     fclose(feco);

    if( options.Showhash ) {
        fprintf( stdout, "Statistics for Equation table\n");
        sym_stat( Eqntp, stdout);

        fprintf( stdout, "Statistics for Symbol table\n");
        sym_stat( Stp, stdout);
    }
#endif

reset();
return 0;
}

char    *mkfname( char *fname, char *path, char *base, char *ext)
{
    char    *fn;

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
            fprintf(fzzout, "%-32s ", sp->name);
            print_dependencies(fzzout, eq->deps);
        } 
    }
    return NXTSYM;
}

/* do_dep: output dependency information for all equations in the symbol table */
static void do_dep(FILE *fzout) {
    fzzout = fzout;
    sym_walk(Eqntp, dep_out, NULL );
}

#ifndef MCISIS

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
#endif
