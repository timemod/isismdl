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
#define MAXLINE 2000 /* maximum length of line that is stored in the buffer */
#define BUF_SIZE (MAXLINE + 1)

static void  open_buffer(char *filename);
static void  close_buffer(void);
static void  close_preproc_file(void);
static void  get_comment (void);
static char* get_includefile ( void );
static int   mcgetc(void);
static void  push_if_stack(int branch_taken);
static void  pop_if_stack(void);
static void  enter_get_flag(int directive);
static void  enter_skip();
static void  enter_initial(void);

/* variables for the buffers */
typedef struct _selem {
    FILE   *fin;
    char   *filename;
    char   *dirname;
    char   *buf;
    char   *bufp;
    size_t line_number;
} buffer_stack_elem ;
#define BUFFER_STACK_SIZE 10
static buffer_stack_elem buffer_stack[BUFFER_STACK_SIZE];
/* Buffer_level: 
 *     -1  no buffer
 *      0  main mdl file
 *    > 0  included mdl files */
static int buffer_level = -1;

static FILE *fin  = NULL;              /* input  stream */
static int write_preproc_file = 0;
static FILE *fp_preproc = NULL;  /* output stream for preprocessed file */
static char *filename;
static char *dirname;
static char *buf;
static char *bufp;
static unsigned int line_number;
static int prev_was_newline = 0;

/* ---------- conditional-compile stack (per-#IF state) ---------- */
#define MAX_IF_DEPTH 10

typedef struct {
  int branch_taken;        /* 1 if some branch (#IF or #ELSEIF or #ELSE) of this #IF was taken */
  int else_seen;           /* 1 if an #ELSE has already been used (error if repeated) */
  unsigned int start_line; /* line where this #IF was seen (for error messages) */
  char *filename; 
} if_state_t;

static if_state_t if_stack[MAX_IF_DEPTH];
static int if_level = -1;            /* -1 == not inside any #IF */

static int pending_directive = 0;    /* DIR_NONE, DIR_IF, DIR_ELSEIF */
enum { DIR_NONE = 0, DIR_IF = 1, DIR_ELSEIF = 2 };

 /* index of the if_level that started the current skip 
    all if statements with higher levels are also skipped */
static int skip_target_level = -1;  

static inline void write_preproc_text(const char *text) {
    if (write_preproc_file) fputs(text, fp_preproc);
}

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
<incl>{filename}  {   
                     char *inclname = get_includefile();
                     if (inclname != NULL) {
                         open_buffer(inclname);
                     } 
                     BEGIN(INITIAL);
                   }

<incl>.            { 
                      mcerror("Illegal #include syntax");
                      BEGIN(INITIAL);
                   }

{if}               { 
                     enter_get_flag(DIR_IF);
                   }

{elseif}           { 
                     if (if_level < 0) {
                       mcerror("Stray #ELSEIF without matching #IF at line %u of %s",
                                (unsigned) line_number, filename);
                     } else if (if_stack[if_level].else_seen) {
                        mcerror("#ELSEIF after #ELSE is not allowed at line %u of %s",
                                (unsigned) line_number, filename);
                     } else  {
                       enter_skip();
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
                       enter_skip();
                     }
                   }

{endif}            {
                     if (if_level < 0) {
                       mcerror("Stray #ENDIF without matching #IF at line %u of %s",
                               (unsigned) line_number, filename);
                     } else {
                       pop_if_stack();
                     }
                   }

<get_flag>[ \t]*   
 <get_flag>{name}   {
                      if (pending_directive == DIR_IF) {
                        int cond = flag_present(mctext);
                        push_if_stack(cond);
                        if (cond) {
                           enter_initial();
                        } else {
                           enter_skip();
                        }
                      } else if (pending_directive == DIR_ELSEIF) {
                        int cond = flag_present(mctext);
                        if_stack[if_level].branch_taken = cond;
                        if (cond) {
                          enter_initial();
                        } else {
                          enter_skip();
                        }
                      } else {
                        mcerror("Internal scanner state error: got name but no pending directive");
                        enter_initial();
                      }
                   }

<skip>{if}       {
                   push_if_stack(0);
                 }

<skip>{elseif}   {
                   if (if_level < 0) {
                     mcerror("Stray #ELSEIF without matching #IF at line %u of %s",
                             (unsigned) line_number, filename);
                   } else if (if_stack[if_level].else_seen) {
                      mcerror("#ELSEIF after #ELSE is not allowed at line %u of %s",
                              (unsigned) line_number, filename);
                   } else if (if_level == skip_target_level &&
                              !if_stack[if_level].branch_taken) {
                      enter_get_flag(DIR_ELSEIF);
                   }
                }

<skip>{else}    {
                  if (if_level < 0) {
                    mcerror("Stray #ELSE without matching #IF at line %u of %s",
                            (unsigned) line_number, filename);
                  } else if (if_stack[if_level].else_seen) {
                     mcerror("Multiple #ELSE in same #IF at line %u of %s",
                              (unsigned) line_number, filename);
                  } else {
                    if_stack[if_level].else_seen = 1;
                    if (if_level == skip_target_level &&
                        !if_stack[if_level].branch_taken) {
                       if_stack[if_level].branch_taken = 1;
                       enter_initial();
                    }
                  }
                }

<skip>{endif}   {
                  if (if_level < 0) {
                    mcerror("Stray #ENDIF without matching #IF at line %u of %s",
                            (unsigned) line_number, filename);
                  } else if (if_level == skip_target_level) {
                    pop_if_stack();
                    enter_initial();
                  } else {
                    pop_if_stack();
                  }
                }

<skip>.         
<skip>\n        

<<EOF>> {
          if (buffer_level == 0) { 
            /* EOF in main mdl file, terminate the scanner */
            if (if_level >= 0) {
               mcerror("Unmatched #IF: missing %d #ENDIF(s) at EOF (last opened at line %u of %s)",
                       if_level + 1, if_stack[if_level].start_line, 
                       if_stack[if_level].filename);
            }
            close_preproc_file();
            yyterminate();
          } else {
            /* EOF in included file: close the current buffer and continue with the
             * previous buffer */
            close_buffer();
          }
        }

{inum}  { write_preproc_text(mctext); mclval.ival = strtol(mctext, NULL, 10); return T_INTNUM; }
{fnum}  { write_preproc_text(mctext); mclval.dval = strtod(mctext, NULL)    ; return T_NUMBER; }
"?"    { write_preproc_text(mctext); get_comment();}
[-=\[\]():;,+*/] { write_preproc_text(mctext); return mclval.ival = mctext[0]; }
"**"      { write_preproc_text(mctext); return mclval.ival = T_POW; }
\.GT\.    { write_preproc_text(mctext); return mclval.ival = T_GT;  }
\.GE\.    { write_preproc_text(mctext); return mclval.ival = T_GE;  }
\.LT\.    { write_preproc_text(mctext); return mclval.ival = T_LT;  }
\.LE\.    { write_preproc_text(mctext); return mclval.ival = T_LE;  }
\.AND\.   { write_preproc_text(mctext); return mclval.ival = T_AND; }
\.OR\.    { write_preproc_text(mctext); return mclval.ival = T_OR;  }
\.NOT\.   { write_preproc_text(mctext); return mclval.ival = T_NOT; }
\.EQ\.    { write_preproc_text(mctext); return mclval.ival = T_EQ;  }
\.NE\.    { write_preproc_text(mctext); return mclval.ival = T_NE;  }
">"       { write_preproc_text(mctext); return mclval.ival = T_GT;  }
">="      { write_preproc_text(mctext); return mclval.ival = T_GE;  }
"<"       { write_preproc_text(mctext); return mclval.ival = T_LT;  }
"<="      { write_preproc_text(mctext); return mclval.ival = T_LE;  }
"^"       { write_preproc_text(mctext); return mclval.ival = T_NOT; }
"^="      { write_preproc_text(mctext); return mclval.ival = T_NE;  }
"&"       { write_preproc_text(mctext); return mclval.ival = T_AND; }
"|"       { write_preproc_text(mctext); return mclval.ival = T_OR;  }

{name}  {       write_preproc_text(mctext);
                /*
                 * lookup symbol and if it does not exist
                 * then give it type T_NAME 
                 */
                Symbol *sp;
                sp = sym_create(Stp, (char *) mctext, T_NAME);
                mclval.sp = sp;
                if (buffer_level > 0 && sp->type == T_END) {
                   mcerror("end keyword not allowed in included file");
                }        
                return sp->type;
        }

[\r]          ; 

[ \v\f\t\n]  write_preproc_text(mctext);
             /* skip other whitespace and newline */

.       {if (!scanning) mcerror( "Illegal character in input" );}

%%
int mcwrap(void) {
  return 1;
}

/*
 * Initialise the scanner for the specified filename 
 */
void init_scanner(char *filenm, const char *filenm_preproc) {
   buffer_level = -1;
   if_level = -1;
   pending_directive = DIR_NONE;
   skip_target_level = -1;

   /* Open the mdl file in the buffer */
   open_buffer(filenm);

   /* Open the pproc file */
   write_preproc_file = filenm_preproc != NULL;
   if (write_preproc_file) {
       fp_preproc = efopen(filenm_preproc, "w+");
   }

   /* start in the base scanning condition */
   BEGIN(INITIAL);
}

void close_preproc_file(void) {
    if (write_preproc_file) {
      fclose(fp_preproc);
      write_preproc_file = 0;
    }
}

/* 
 * Closes the scanner:
 *    - close the pproc file
 *    - close buffers and deallocate memory
 *    - clean up memory
 */
void close_scanner(void) {

    if (write_preproc_file) {
      /* write a newline, the newline after end; in the input is ignored */
      fputc('\n', fp_preproc);
      close_preproc_file();
    }

    /* reset conditional stack */
    if_level = -1;
    pending_directive = DIR_NONE;
    skip_target_level = -1;

    /* Reset scanner state */
    BEGIN(INITIAL);

    /* First close the current buffer */
    close_buffer();

    /* Close remaining buffers on the stack */
    if (buffer_level >= 0) {
      int i;
      for (i = 0; i <= buffer_level; i++) {
        fclose(buffer_stack[i].fin);
        free(buffer_stack[i].dirname);
        free(buffer_stack[i].buf);
        buffer_stack[i].fin = NULL;
        buffer_stack[i].dirname = NULL;
        buffer_stack[i].buf = NULL;
        buffer_stack[i].filename = NULL;
        /* WARNING: do not free filename, a reference to filename
         * may still exist on the include stack and may be needed for
         * error messages. The allocated filenames for inluced files
         * will be freed by function reset_include_paths(). */
      }
    }
    buffer_level = -1;
}

/*
 * open a new buffer for reading from the specified filename
 */
void open_buffer(char *new_filename) {   
  
    /* Save the current buffer on the stack */
    if (buffer_level >= 0) {

        /* Check if the buffer_stack is long enough.
         * Note that most recently opened buffer is not stored on stack,
         * a buffer will only be stored on the stack when a new buffer is openend.
         * Therefore the maximum include depth is equal to BUFFER_STACK_SIZE,
         * even though the maximum include depth on the stack is BUFFER_STACK_SIZE - 1.
         */
        if (buffer_level >= BUFFER_STACK_SIZE) {
            mcerror("Includes nested too deeply. Maximum include depth %d.", 
                    BUFFER_STACK_SIZE);
           return;
        }
        buffer_stack[buffer_level].fin         = fin;
        buffer_stack[buffer_level].filename    = filename;
        buffer_stack[buffer_level].dirname     = dirname;
        buffer_stack[buffer_level].buf         = buf;
        buffer_stack[buffer_level].bufp        = bufp;
        buffer_stack[buffer_level].line_number = line_number;
    }

    /* Create a new buffer */
    filename = new_filename;
    dirname = get_path_name(filename);
    fin = efopen(filename, "r");
    buf = emalloc(BUF_SIZE);
    buf[0] = 0;
    bufp = buf;
    line_number = 1;
    prev_was_newline = 0;

    /* increase buffer level */
    buffer_level++;
} 

/* Closes the current buffer and restores the previous buffer */
void close_buffer(void) {
 
    if (buffer_level < 0) {
      return;
    }

    /* Close buffer and free memory */
    fclose(fin);
    free(buf);
    fin = NULL;
    buf = NULL;
    bufp = NULL;
    line_number = 0;
    prev_was_newline = 0;

    free(dirname); /* dirname was allocated in get_path_name */
    dirname = NULL;

    /* Restore previous buffer */
    if (--buffer_level >= 0) { 	
        fin         = buffer_stack[buffer_level].fin;
        filename    = buffer_stack[buffer_level].filename;
        dirname     = buffer_stack[buffer_level].dirname;
        buf         = buffer_stack[buffer_level].buf;
        bufp        = buffer_stack[buffer_level].bufp;
        line_number = buffer_stack[buffer_level].line_number;
    } 
}

/* return next char from current buffer or EOF */
static int mcgetc(void) {   
    int     c;
    char    *pn;
    if (bufp == NULL) {
        return EOF;
    }
    if (*bufp == 0) {
        if ((bufp = fgets(buf, BUF_SIZE, fin)) == NULL)
            return EOF;
        pn = strchr(bufp, '\n');
        if (pn != NULL) {
            /* remove any \r before \n */
            if (pn > bufp && *(pn - 1) == '\r') {
                *(pn - 1) = '\n';
                *pn       = '\0';
            }
        } else {
            /* no newline found, check for \r at end of buffer content */
            size_t len = strlen(bufp);
            if (len > 0 && bufp[len - 1] == '\r') {
                bufp[len - 1] = '\0';
            }
        }
    }
    c = *bufp++;
    if (prev_was_newline) {
        line_number++;
        prev_was_newline = 0;
    }
    if (c == '\n') {
        prev_was_newline = 1;
    }
    return c;
}

void showbuf(FILE *fout) {
    char *p   = buf;

    char *end;
    if (bufp != NULL) {
        end = bufp;
    } else {
        char *pn = strchr(buf, '\n');
        end = pn ? pn : buf + strlen(buf);
    }

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

/* Returns the name of the include file based on the specified
 * include filename and the include search paths.
 */
static char *get_includefile(void) {

   /* extract includename, disgarding the leading and trailing quotes from mctext */
   size_t len = strlen(mctext) - 2;
   char *includename = emalloc(len + 1);
   memcpy(includename, mctext + 1, len);
   includename[len] = '\0';

   /* search for the include file based on the include search paths */
   char *includefile = get_includefilename(includename, dirname);
   if (includefile == NULL) {
       mcerror("Include file %s not found", includename);
   }
   free(includename);
   return includefile;
}

static void push_if_stack(int branch_taken) {
    if (if_level >= MAX_IF_DEPTH - 1) {
        mcerror("#IF stack overflow (max %d) at line %u of %s, parsing terminated", MAX_IF_DEPTH,
                (unsigned) line_number, filename);
        stop_parsing();
        return;
    }

    if_level++;
    if_stack[if_level].branch_taken = branch_taken;
    if_stack[if_level].else_seen    = 0;
    if_stack[if_level].start_line   = line_number;
    if_stack[if_level].filename = filename;
}

static void pop_if_stack(void) {

  if (if_level < 0) return;

  if_level--;
}

static void enter_get_flag(int directive) {
  pending_directive = directive;
  BEGIN(get_flag);
}

static void enter_skip(void) {
  skip_target_level = if_level;
  pending_directive = DIR_NONE;
  BEGIN(skip);
}

static void enter_initial(void) {
  skip_target_level = -1;
  pending_directive = DIR_NONE;
  BEGIN(INITIAL);
}
