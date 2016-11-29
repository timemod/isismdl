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

extern  void    set_standard_path(char *path);
extern  void    add_include_path(char *path);
extern  char    *get_includefilename(char *includename);
extern  void    init_include_dirs(void);

extern  int     mcerror(const char *fmt, ... );
extern  void    mcwarn(const char *fmt, ... );

extern  void    mcerrmessage(const char *, va_list);
extern  void    xpcerrmsg(char *, ... );

extern  void    mc_scan_params(void);
extern  void    export_names(void);

extern Mcopt options;

extern  int     mcexec(char *mfname, Mcopt options);
#endif
