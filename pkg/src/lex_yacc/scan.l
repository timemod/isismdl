/*
 * lexical analyzer for xpc
 */

%{

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
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
#define MAXLINE 256

static void    open_buffer(char *filename);
static void    close_buffer(void);
static void    get_comment ( void );
static char*   get_includefile ( void );
static int     mcgetc(void);

typedef struct _selem {
    FILE   *fin;
    char   *filename;
    char   *dirname;
    char   *buf;
    char   *bufp;
    size_t line_number;
} include_stack_elem ;

static include_stack_elem include_stack[MAX_INCLUDE_DEPTH + 1];

static  FILE *fin  = NULL;              /* input  stream */
static char *filename;
static char *dirname;
static char *buf;
static char *bufp;
static int include_level = -1;
static unsigned int line_number;
static size_t if_nest_cnt; /* number of nested #if constructs within a #if */

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

#define YY_FATAL_ERROR(msg) error(msg)

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
%x goto_else
%x goto_endif

filename \".+\"
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
<incl>[ \t]*        /* eat the white space */
<incl>{filename} {   
                    char *inclname = get_includefile();
                    if (inclname != NULL) {
                        open_buffer(inclname);
                    }
                    BEGIN(INITIAL);
                 }

{if}               BEGIN(get_flag);
{elseif}           {if_nest_cnt = 0;
                    BEGIN(goto_endif);
                   }
{else}             {if_nest_cnt = 0;
                    BEGIN(goto_endif);
                   }
{endif}            /* ignore */

<get_flag>[ \t]*   /* eat the white space */
<get_flag>{name}   {  if (flag_present(mctext)) {
		   	BEGIN(INITIAL);
                      } else {
                        if_nest_cnt = 0;
			BEGIN(goto_else);
                      }
                   }

<goto_else>{if}      {if_nest_cnt++;}
<goto_else>{elseif}  {if (!if_nest_cnt) BEGIN(get_flag);}
<goto_else>{else}    {if (!if_nest_cnt) BEGIN(INITIAL);}
<goto_else>{endif}   {  if (!if_nest_cnt) {
                            BEGIN(INITIAL);
                        } else {
                          if_nest_cnt--;
                        }
                     }
<goto_else>.         /* skip */
<goto_else>\n        /* skip newline */

<goto_endif>{if}      {if_nest_cnt++;}
<goto_endif>{endif}   {  if (!if_nest_cnt) { 
                             BEGIN(INITIAL);
                         } else {
                            if_nest_cnt--;
                         }
                      }
<goto_endif>.         /* skip */
<goto_endif>\n        /* skip newline */

<<EOF>> { close_buffer();
          if (include_level < 0) yyterminate();}

{inum}  { mclval.ival = strtol(mctext, NULL, 10); return T_INTNUM; }
{fnum}  { mclval.dval = strtod(mctext, NULL)    ; return T_NUMBER; }

"?"    { get_comment(); }

[-=\[\]():;,+*/] { return mclval.ival = mctext[0]; }

"**"      { return mclval.ival = T_POW; }
\.GT\.    { return mclval.ival = T_GT;  }
\.GE\.    { return mclval.ival = T_GE;  }
\.LT\.    { return mclval.ival = T_LT;  }
\.LE\.    { return mclval.ival = T_LE;  }
\.AND\.   { return mclval.ival = T_AND; }
\.OR\.    { return mclval.ival = T_OR;  }
\.NOT\.   { return mclval.ival = T_NOT; }
\.EQ\.    { return mclval.ival = T_EQ;  }
\.NE\.    { return mclval.ival = T_NE;  }

">"       { return mclval.ival = T_GT;  }
">="      { return mclval.ival = T_GE;  }
"<"       { return mclval.ival = T_LT;  }
"<="      { return mclval.ival = T_LE;  }
"^"       { return mclval.ival = T_NOT; }
"^="      { return mclval.ival = T_NE;  }
"&"       { return mclval.ival = T_AND; }
"|"       { return mclval.ival = T_OR;  }

{name}  {
                Symbol *sp;

                /*
                 * lookup symbol and if it does not exist
                 * then give it type T_NAME 
                 */

                sp = sym_create(Stp, (char *) mctext, T_NAME);

                mclval.sp = sp;

                return sp->type;
        }


[ \r\v\f\t\n]  ;       /* skip whitespace, including spurious carriage return */

.       {if (!scanning) mcerror( "Illegal character in input" );}


%%

int mcwrap( void )
{
    return 1;
}

void init_scanner(char *filenm) {
    /*
     * Initialise the scanner for the specified filename 
     */

    include_level = -1;

    open_buffer(filenm);
}

void reset_scanner(void) {
    /*
     * Close all buffers. This function should be called when an
     * end; statement has occurred in the model
     */

    /* first close current buffer (the current buffer is not stored on 
     * the stack) */
    close_buffer();

    if (include_level == -1) return;

    /* close buffers on the stack */
    int i;
    for (i = 0; i <= include_level; i++) {
        fclose(include_stack[include_level].fin);
        free(include_stack[include_level].filename);
        free(include_stack[include_level].dirname);
        free(include_stack[include_level].buf);
    }

    include_level = -1;
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

    /* The first path to be searched for include files should be the
     * directory of the current include file */
    set_standard_path(dirname);
}

static void close_buffer(void) {

   /* Closes the current buffer */

    fclose(fin);
    free(buf);
    bufp = NULL;
    if (include_level > 0) {
        /* filename was allocated with malloc in get_includefile(). */
	free(filename);
    }
    free(dirname);
    
    if (--include_level >= 0) {	
        /* restore previous buffer information */

        fin         = include_stack[include_level].fin;
        filename    = include_stack[include_level].filename;
        dirname     = include_stack[include_level].dirname;
        buf         = include_stack[include_level].buf;
        bufp        = include_stack[include_level].bufp;
        line_number = include_stack[include_level].line_number;

	/* make directory of current file the first directory to be searched
         * for include files */ 
        set_standard_path(dirname);
    } 
}

static int mcgetc(void)
{   
     /* return next char from current buffer
     * or EOF
     */

    int     c;
    char    *pn;

    if( bufp == NULL)
        return EOF;

    if( *bufp == 0 )
    {
        if( (bufp = fgets(buf, MAXLINE-2, fin)) == NULL )
            return EOF;
        else if( (pn = strchr(bufp, '\n')) == NULL )
//            warning("Line too long and split\n");

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

void showbuf( FILE *fout )
{
    char    *p   = buf;
    char    *end = bufp ? bufp : strchr(buf,'\n') - 1;
    int pos = 0;

    for( ; *p && p < end ; p++)
    {
        if( *p == '\t')
            pos += 8 - (pos % 8);
        else
            pos++;
    }

    if( line_number ) 
        (void) fprintf(fout,"Line: %u of file %s\n", line_number, filename);
    fprintf(fout, "%s", buf);
    fprintf(fout, "%*s\n", pos, "^" );
}


/*
 * read comment
 * and discard to end of line
 */

static  void    get_comment( void )
{
    int     c;

    while( (c = input()) != '\n' && c != EOF);
}

static char * get_includefile(void) 
{
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
   includefile = get_includefilename(includename);
   free(includename);
   if (includefile == NULL) {
       mcerror("Include file %s not found", includename);
   }
   return includefile;
}
