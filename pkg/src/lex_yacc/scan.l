/*
 * lexical analyzer for xpc
 */
%{
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <Rinternals.h>
#include "symbol.h"
#include "ecode.h"
#include "mcinit.h"
#include "util.h"
#include "mchdr.h"
#include "fname.h"
#include "flags.h"
#include "xpcdef.h"
#include "xpctab.h"
#define MAX_INCLUDE_DEPTH 10
#define MAXLINE 2000
static void  open_buffer(char *filename);
static void  close_buffer(void);
static void  close_preproc_file(void);
static void  get_comment (void);
static char* get_includefile ( void );
static int   mcgetc(void);
static int   push_if_stack(int branch_taken);

typedef struct _selem {
    FILE   *fin;
    char   *filename;
    char   *dirname;
    char   *buf;
    char   *bufp;
    size_t line_number;
} include_stack_elem ;
static include_stack_elem include_stack[MAX_INCLUDE_DEPTH + 1];
static FILE *fin  = NULL;              /* input  stream */
static int write_preproc_file = 0;
static FILE *fp_preproc = NULL;  /* output stream for preprocessed file */
static char *filename;
static char *dirname;
static char *buf;
static char *bufp;
static int include_level = -1;
static unsigned int line_number;

/* ---------- conditional-compile stack (per-#IF state) ---------- */
#define MAX_IF_DEPTH 10

typedef struct {
  int branch_taken;     /* 1 if some branch (#IF or #ELSEIF or #ELSE) of this #IF was taken */
  int else_seen;        /* 1 if an #ELSE has already been used (error if repeated) */
  int skipping;         /* 1 if the current active branch at this level is being skipped */
  unsigned int start_line; /* line where this #IF was seen (for error messages) */
} if_state_t;

static if_state_t if_stack[MAX_IF_DEPTH];
static int if_level = -1;            /* -1 == not inside any #IF */

static int pending_directive = 0;    /* DIR_NONE, DIR_IF, DIR_ELSEIF */
enum { DIR_NONE = 0, DIR_IF = 1, DIR_ELSEIF = 2 };

static int skip_target_level = -1;   /* index of the if_level that started the current skip */

/* --------------------------------------------------------------- */

/*
 * for MKS Lex
 */
#undef  YY_INTERACTIVE
#define YY_INTERACTIVE 0
/*
 * the following is needed
 * to make flex not use isatty and fileno
 * which gave errors in fcc (Fujitsu C compiler in Lahey)
 */
#define YY_NEVER_INTERACTIVE 1
/*
 * Flex needs this
 * MKS Lex doesn't and just ignores this
 * but do use the special Flex scanner macro
 */
#ifdef FLEX_SCANNER
#define YY_INPUT(buf,result,max_size) \
{\
	int c = mcgetc(); \
	result = (c == EOF) ? YY_NULL : (buf[0] = c, 1); \
}
#endif
%}
/* the "incl" state is used for picking up the name of an include file
 */
%x incl
%x get_flag
%x skip
filename \"[^\n\r\"]+\"
name     [a-zA-Z][a-zA-Z0-9#@%_]*
D        [0-9]
fnum     ({D}+\.?{D}*|\.{D}+)([eE][+-]?{D}+)?
inum     {D}+
include  "#INCLUDE"
if       "#IF"
else     "#ELSE"
elseif   "#ELSEIF"
endif    "#ENDIF"


%%
{include}          BEGIN(incl);
<incl>[ \t]*        
<incl>{filename} {   
                    char *inclname = get_includefile();
                    if (inclname != NULL) {
                        open_buffer(inclname);
                    } 
                    BEGIN(INITIAL);
                 }
<incl>.       { mcerror("Illegal #include syntax");
                BEGIN(INITIAL);
              }

{if}               {
                     pending_directive = DIR_IF;
                     BEGIN(get_flag);
                   }

{elseif}           { 
                     if (if_level < 0) {
                       mcerror("Stray #ELSEIF without matching #IF at line %u of %s",
                               (unsigned) line_number, filename);
                     } else if (if_stack[if_level].branch_taken) {
                       skip_target_level = if_level;
                       BEGIN(skip);
                     } else {
                       pending_directive = DIR_ELSEIF;
                       BEGIN(get_flag);
                     }
                   }

{else}             {
                     if (if_level < 0) {
                       mcerror("Stray #ELSE without matching #IF at line %u of %s",
                                (unsigned) line_number, filename);
                     } else if (if_stack[if_level].else_seen) {
                       mcerror("Multiple #ELSE in same #IF at line %u of %s",
                                (unsigned) line_number, filename);
                     } else {
                       if_stack[if_level].else_seen = 1;
                       if (if_stack[if_level].branch_taken) {
                         skip_target_level = if_level;
                         BEGIN(skip);
                       } else {
                         if_stack[if_level].branch_taken = 1;
                         if_stack[if_level].skipping = 0;
                         BEGIN(INITIAL);
                       }
                     }
                   }

{endif}            {
                     if (if_level < 0) {
                       mcerror("Stray #ENDIF without matching #IF at line %u of %s",
                               (unsigned) line_number, filename);
                     } else {
                       if_level--;
                       if (if_level < 0) {
                         BEGIN(INITIAL);
                       } else {
                         if (if_stack[if_level].skipping) {
                           skip_target_level = if_level;
                           BEGIN(skip);
                         } else {
                           BEGIN(INITIAL);
                         }
                       }
                     }
                   }

<get_flag>[ \t]*   
 <get_flag>{name}   {
                        if (pending_directive == DIR_IF) {
                            int cond = flag_present(mctext);
                            if (push_if_stack(cond ? 1 : 0)) yyterminate();
                            pending_directive = DIR_NONE;

                          if (if_stack[if_level].skipping) {
                            skip_target_level = if_level;
                            BEGIN(skip);
                          } else {
                            BEGIN(INITIAL);
                          }
                      } else if (pending_directive == DIR_ELSEIF) {
                          if (if_level < 0) {
                            mcerror("Internal: #ELSEIF without #IF at line %u of %s",
                                     (unsigned) line_number, filename);
                            pending_directive = DIR_NONE;
                            BEGIN(INITIAL);
                          } else if (if_stack[if_level].else_seen) {
                            mcerror("#ELSEIF after #ELSE is not allowed at line %u of %s",
                                     (unsigned) line_number, filename);
                            pending_directive = DIR_NONE;
                            BEGIN(INITIAL);
                          } else if (if_stack[if_level].branch_taken) {
                            pending_directive = DIR_NONE;
                            skip_target_level = if_level;
                            BEGIN(skip);
                          } else {
                            int cond = flag_present(mctext);
                            if_stack[if_level].branch_taken = cond ? 1 : 0;
                            if_stack[if_level].skipping     = cond ? 0 : 1;
                            pending_directive = DIR_NONE;
                            if (if_stack[if_level].skipping) {
                              skip_target_level = if_level;
                              BEGIN(skip);
                            } else {
                              BEGIN(INITIAL);
                            }
                          }
                      } else {
                          mcerror("Internal scanner state error: got name but no pending directive");
                          BEGIN(INITIAL);
                      }
                   }

<skip>{if}      {
                    if (push_if_stack(0)) yyterminate();
                  }

<skip>{elseif}  {
                  if (if_level < 0) {
                    mcerror("Stray #ELSEIF without matching #IF at line %u of %s",
                            (unsigned) line_number, filename);
                  } else if (if_level == skip_target_level) {
                    if (if_stack[if_level].branch_taken) {
                    } else {
                      pending_directive = DIR_ELSEIF;
                      BEGIN(get_flag);
                    }
                  } else {
                  }
                }

<skip>{else}    {
                  if (if_level < 0) {
                    mcerror("Stray #ELSE without matching #IF at line %u of %s",
                            (unsigned) line_number, filename);
                  } else if (if_level == skip_target_level) {
                    if (if_stack[if_level].else_seen) {
                      mcerror("Multiple #ELSE in same #IF at line %u of %s",
                              (unsigned) line_number, filename);
                    } else {
                      if_stack[if_level].else_seen = 1;
                      if (if_stack[if_level].branch_taken) {
                      } else {
                        if_stack[if_level].branch_taken = 1;
                        if_stack[if_level].skipping = 0;
                        skip_target_level = -1;
                        BEGIN(INITIAL);
                      }
                    }
                  } else {
                  }
                }

<skip>{endif}   {
                  if (if_level < 0) {
                    mcerror("Stray #ENDIF without matching #IF at line %u of %s",
                            (unsigned) line_number, filename);
                  } else if (if_level == skip_target_level) {
                    if_level--;
                    if (if_level < 0) {
                      skip_target_level = -1;
                      BEGIN(INITIAL);
                    } else if (if_stack[if_level].skipping) {
                      skip_target_level = if_level;
                      BEGIN(skip);
                    } else {
                      skip_target_level = -1;
                      BEGIN(INITIAL);
                    }
                  } else {
                    if_level--;
                  }
                }

<skip>.         
<skip>\n        

<<EOF>> {
          if (include_level <= 0) {
            close_preproc_file();
            if (if_level >= 0) {
               mcerror("Unmatched #IF: missing %d #ENDIF(s) at EOF (last opened at line %u of %s)",
                       if_level + 1, if_stack[if_level].start_line, filename);
            }
            yyterminate();
          } else {
            /* close the current buffer and restore the previous buffer */
            close_buffer();
          }
        }

{inum}  {if (write_preproc_file) fputs(mctext, fp_preproc); mclval.ival = strtol(mctext, NULL, 10); return T_INTNUM; }
{fnum}  {if (write_preproc_file) fputs(mctext, fp_preproc); mclval.dval = strtod(mctext, NULL)    ; return T_NUMBER; }
"?"    {if (write_preproc_file) fputs(mctext, fp_preproc); get_comment();}
[-=\[\]():;,+*/] {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = mctext[0]; }
"**"      {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_POW; }
\.GT\.    {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_GT;  }
\.GE\.    {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_GE;  }
\.LT\.    {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_LT;  }
\.LE\.    {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_LE;  }
\.AND\.   {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_AND; }
\.OR\.    {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_OR;  }
\.NOT\.   {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_NOT; }
\.EQ\.    {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_EQ;  }
\.NE\.    {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_NE;  }
">"       {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_GT;  }
">="      {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_GE;  }
"<"       {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_LT;  }
"<="      {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_LE;  }
"^"       {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_NOT; }
"^="      {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_NE;  }
"&"       {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_AND; }
"|"       {if (write_preproc_file) fputs(mctext, fp_preproc); return mclval.ival = T_OR;  }
{name}  {       if (write_preproc_file) fputs(mctext, fp_preproc);
                Symbol *sp;
                /*
                 * lookup symbol and if it does not exist
                 * then give it type T_NAME 
                 */
                sp = sym_create(Stp, (char *) mctext, T_NAME);
                mclval.sp = sp;
                return sp->type;
        }
[\r]          ; 
[ \v\f\t\n]  if (write_preproc_file) fputs(mctext, fp_preproc);
             /* skip other whitespace and newline */
.       {if (!scanning) mcerror( "Illegal character in input" );}
%%
int mcwrap(void) {
  return 1;
}
void init_scanner(char *filenm, const char *filenm_preproc) {
   /*
    * Initialise the scanner for the specified filename 
    */
   include_level = -1;
   if_level = -1;
   pending_directive = DIR_NONE;
   skip_target_level = -1;
   open_buffer(filenm);
   write_preproc_file = filenm_preproc != NULL;
   if (write_preproc_file) fp_preproc = fopen(filenm_preproc, "w+");
   /* start in the base scanning condition */
   BEGIN(INITIAL);
}
void close_preproc_file(void) {
    if (write_preproc_file) {
      fclose(fp_preproc);
      write_preproc_file = 0;
    }
}
void reset_scanner(void) {
     
    //Rprintf("reset scanner\n");
    /*
     * Close all buffers. This function is called when an
     * end; statement has occurred in the model
     */
    if (write_preproc_file) {
      // write a newline, the newline after end; in the input is ignored
      fputc('\n', fp_preproc);
      close_preproc_file();
    }
    if (include_level > 0) {
      mcerror("end; statement not allowed in included file");
    }
    /* reset conditional stack */
    if_level = -1;
    pending_directive = DIR_NONE;
    skip_target_level = -1;
    /* first close current buffer (the current buffer is not stored on 
     * the stack) */
    close_buffer();
    if (include_level == -1) return;
    /* close buffers on the stack */
    int i;
    for (i = 0; i <= include_level; i++) {
        fclose(include_stack[i].fin);
        if (i > 0) {
          /* filename was allocated with malloc in get_includefile(). */
          free(include_stack[i].filename);
        }
        free(include_stack[i].dirname);
        free(include_stack[i].buf);
    }
    include_level = -1;
    /* is dit nodig? uitzoeken */
    BEGIN(INITIAL);
}
void open_buffer(char *new_filename) {   
    /*
     * open a new buffer for reading from the specified filename
     */
    if (include_level >= 0) {
	      /*
         * save current buffer information on stack
         */
        include_stack[include_level].fin         = fin;
        include_stack[include_level].filename    = filename;
        include_stack[include_level].dirname     = dirname;
        include_stack[include_level].buf         = buf;
        include_stack[include_level].bufp        = bufp;
        include_stack[include_level].line_number = line_number;
    }
    if (++include_level > MAX_INCLUDE_DEPTH) {
        mcerror("Includes nested too deeply. Maximum include depth %d.", 
                MAX_INCLUDE_DEPTH);
        return;
    }
    /* create a new buffer */
    filename = new_filename;
    dirname = get_path_name(filename);
    fin = efopen( filename, "r" );
    buf = emalloc(MAXLINE + 1);
    buf[0] = 0;
    bufp = buf;
    line_number = 0;
}
static void close_buffer(void) {
   /* Closes the current buffer */
    fclose(fin);
    free(buf);
    bufp = NULL;
    /* if include_level = 0: filename is a pointer to the local stack variable fname in mcexec. */
    /* if include_level > 0: filename was allocated with malloc in get_includefilename(). */
    if (include_level > 0) {
      	free(filename);
    }
    free(dirname); /* dirname was allocated in get_path_name */
    if (--include_level >= 0) { 	
        /* restore previous buffer information */
        fin         = include_stack[include_level].fin;
        filename    = include_stack[include_level].filename;
        dirname     = include_stack[include_level].dirname;
        buf         = include_stack[include_level].buf;
        bufp        = include_stack[include_level].bufp;
        line_number = include_stack[include_level].line_number;
    } 
}

/* return next char from current buffer
 * or EOF
 */
static int mcgetc(void) {   
    int     c;
    char    *pn;
    if( bufp == NULL)
        return EOF;
    if (*bufp == 0) {
        if( (bufp = fgets(buf, MAXLINE-2, fin)) == NULL )
            return EOF;
        else if( (pn = strchr(bufp, '\n')) == NULL )
           // warning("Line too long and split\n");
        /*
         * remove any \r at end
         */
        if( pn && pn > bufp && *(pn-1) == '\r' )
        {
            *(pn-1) = '\n';
            *pn     = '\0';
        }
        ++line_number;
    }
    c = *bufp++;
    return c;
}

void showbuf(FILE *fout) {
    char    *p   = buf;
    char    *end = bufp ? bufp : strchr(buf,'\n') - 1;
    int pos = 0;
    for ( ; *p && p < end ; p++) {
        if (*p == '\t')
            pos += 8 - (pos % 8);
        else
            pos++;
    }
    if (line_number) 
        (void) fprintf(fout,"Line: %u of file %s\n", line_number, filename);
    fprintf(fout, "%s", buf);
    fprintf(fout, "%*s\n", pos, "^");
}

/*
 * Read comment and discard to end of line
 */
static void get_comment(void) {
    int c;
    while ((c = input()) != '\n' && c != EOF) {
       /* Do not write \r (carriage return) to the preproc file.
        * On Windows, a \r is automatically added for every \n when the
        * file is written, so if we would write a \r the result would be
        * \r\r\n.
        */
       if (write_preproc_file && c != '\r') fputc((char) c, fp_preproc);
    }
    if (write_preproc_file) fputc('\n', fp_preproc);
}

static char * get_includefile(void) {
   /* Returns the name of the include file based on the specified
    * include filename and the include search paths
    */
   /* remove the quotes from mctext */
   char *includefile;
   int len = strlen(mctext) - 2;
   char *includename = emalloc(len + 1);
   includename = strncpy(includename, mctext + 1, len);
   *(includename + len) = 0;
   /* search for the include file based on the include search paths */
   includefile = get_includefilename(includename, dirname);
   if (includefile == NULL) {
       mcerror("Include file %s not found", includename);
   }
    free(includename);
    return includefile;
 }

static int push_if_stack(int branch_taken) {
    if (if_level >= MAX_IF_DEPTH - 1) {
        mcerror("#IF stack overflow (max %d) at line %u of %s, parsing terminated", MAX_IF_DEPTH,
                (unsigned) line_number, filename);
        return 1;
    }
    if_level++;
    if_stack[if_level].branch_taken = branch_taken;
    if_stack[if_level].else_seen    = 0;
    if_stack[if_level].skipping     = branch_taken ? 0 : 1;
    if_stack[if_level].start_line   = line_number;
    return 0;
}
