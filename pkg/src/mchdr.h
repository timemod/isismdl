/*
 *      globals defined in
 *              scan.l          after processing by lex
 *              xpctab.y        after processing by yacc
 *
 *      prototypes of functions needed by .l and .y files
 */

#ifndef MCHDR_H
#define MCHDR_H

#include "mcopt.h"
#include "include_paths.h"

extern size_t   errcnt;
extern size_t   warncnt;

extern FILE    *mcin;
extern int     mcdebug;
extern int     scanning;

int    mcparse();
void   mcparse_init(void);

int    mclex(void);
void   init_scanner(char *filename, const char *filename_preproc);
void   reset_scanner(void);
void   showbuf( FILE *fout );

void   mcrestart(FILE *);

int    mcerror(const char *fmt, ... );
void   mcwarn(const char *fmt, ... );

void   stop_parsing(void);

void   mcerrmessage(const char *, va_list);
void   xpcerrmsg(char *, ... );

void   mc_scan_params(void);
void   export_names(void);

extern Mcopt options;

int mcexec(const char *mfname, const char *outputfile, 
	 	       const Mcopt *options);

#endif
