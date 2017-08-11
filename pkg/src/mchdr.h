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

extern  size_t   errcnt;
extern  size_t   warncnt;

extern  FILE    *mcin;
extern  int     mcdebug;
extern  int     scanning;

extern  int     mcparse();
extern  void    mcparse_init(void);

extern  int     mclex(void);
extern  void    init_scanner(char *filename);
extern  void    reset_scanner(void);
extern  void    showbuf( FILE *fout );

extern  void    mcrestart(FILE *);

extern  int     mcerror(const char *fmt, ... );
extern  void    mcwarn(const char *fmt, ... );

extern  void    mcerrmessage(const char *, va_list);
extern  void    xpcerrmsg(char *, ... );

extern  void    mc_scan_params(void);
extern  void    export_names(void);

extern Mcopt options;

extern  int     mcexec(char *mfname, Mcopt options);
#endif
