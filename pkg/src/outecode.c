/*
 * output an enode tree in printable form
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "symbol.h"
#include "util.h"
#include "mcinit.h"
#include "ecode.h"
#include "xpcdef.h"
#include "mchdr.h"
#include "mdldef.h"
#include "outecode.h"

#include "oprintf.h"

#if 0

static  FILE    *fout;

static  void xprintf( char *fmt, ... )
{
        int     x;
        va_list ap;

        va_start(ap, fmt);
        x = vfprintf(fout, fmt, ap);
        if( x < 0 )
                fatal("write error on .eco file\n");
        va_end(ap);
}

#endif

static  void printlag(Enode *ep )
{
        xprintf( " Lagtype=%d offset=%d", ep->second.offset, ep->third.offset);
}

static  void printfargs( char *opcode, Enode *ep )
{
        xprintf( "%s name=%s argcnt=%u arglist=%u" ,
               opcode, ep->first.sp->name, ep->second.ep, ep->third.ep);
}

static  void printbinop( char *opcode, Enode *ep )
{
        xprintf( "%s f=%u s=%u" , opcode, ep->first.ep, ep->second.ep);
}

static  void printunop( char *opcode, Enode *ep )
{
        xprintf( "%s f=%u", opcode, ep->first.ep);
}

/* arithmetic binary operators */
static  char *binopname[] =
{
/* E_ADD   */    "E_ADD"  ,
/* E_SUB   */    "E_SUB"  ,
/* E_MUL   */    "E_MUL"  ,
/* E_DIV   */    "E_DIV"  ,
/* E_POW   */    "E_POW"  ,
};


/* logical binary operators */
static char *logbinopname[] = 
{
/* E_LT    */    "E_LT"   ,
/* E_GT    */    "E_GT"   ,
/* E_EQ    */    "E_EQ"   ,
/* E_LE    */    "E_LE"   ,
/* E_GE    */    "E_GE"   ,
/* E_NE    */    "E_NE"   ,
                 "dummy"  ,
                 "dummy"  ,
/* E_OR    */    "E_OR"   ,
/* E_AND   */    "E_AND"  ,
};

static  char *btname[] =
{
/* E_LOG    */  "E_LOG ",
/* E_EXP    */  "E_EXP ",
/* E_ABS    */  "E_ABS ",
/* E_SIN    */  "E_SIN ",
/* E_COS    */  "E_COS ",
/* E_ATAN   */  "E_ATAN",
/* E_SQRT   */  "E_SQRT",
/* E_NINT   */  "E_NINT",
/* E_LOG10  */  "E_LOG10",
/* E_TAN    */  "E_TAN ",
/* E_ASIN   */  "E_ASIN",
/* E_ACOS   */  "E_ACOS",
/* E_SINH   */  "E_SINH",
/* E_COSH   */  "E_COSH",
/* E_TANH   */  "E_TANH",
/* E_TOREAL */  "E_TOREAL",
/* dummy  */     "dummy",
/* dummy    */  "dummy"
/* E_CUMNOR */  "E_CUMNOR",
/* E_INVNOR */  "E_INVNOR",
};

/* built-in functions with two arguments  */
static  char *btname2[] =
{
/* E_HYPOT */   "E_HYPOT",
                "dummy"
/* E_FIBUR */   "E_FIBUR",
                 "dummy"
};

/* min and max */
static  char *btname3[] =
{
/* E_MAX    */  "E_MAX ",
/* E_MIN    */  "E_MIN ",
};

static  void print_enodes( Enode *epbase )
{
        Enode   *ep;

        for( ep = epbase; ep->operator != E_STOP; ep++ )
        {
                xprintf( "%4u : ", ep - epbase );

                switch( ep->operator )
                {
                        case E_RCONST : xprintf( "E_RCONST %.15g",
                                                ep->first.rval); break;

                        case E_MVAR   : xprintf( "E_MVAR %s" ,
                                                ep->first.sp->name);
                                        printlag(ep);
                                        break;

                        case E_SVAR   : xprintf( "E_SVAR" ); break;

                        case E_PARAM  : xprintf( "E_PARAM %s",
                                                ep->first.sp->name );
                                        printlag(ep);
                                        break;

                        case E_SUM    : xprintf( "E_SUM name=%s low=%d high=%d expr=%u",
                                                ep->first.sp->name,
                                                ep->first.sp->u.sumvarp->low,
                                                ep->first.sp->u.sumvarp->high,
                                                ep->second.ep );
                                        break;

                        case E_DEL    : xprintf( "E_DEL offset=%d expr=%u" ,
                                                ep->first.offset,
                                                ep->second.ep);
                                        break;

                        case E_IF     : xprintf( "E_IF c=%u t=%u e=%u" ,
                                                ep->first.ep,
                                                ep->second.ep,
                                                ep->third.ep );
                                        break;

                        case E_ARG    : xprintf( "E_ARG argnum=%u",
                                                ep->first.ep );
                                        printlag(ep);
                                        break;

                        case E_CALL   : printfargs( "E_CALL", ep);
                                        break;

                        case E_ARGLIST: printbinop( "E_ARGLIST", ep);
                                        break;

                        case E_ADD    :
                        case E_SUB    :
                        case E_MUL    :
                        case E_DIV    :
                        case E_POW    : printbinop(binopname[ep->operator-E_ADD],
                                                         ep);
                                        break;

                        case E_LT     :
                        case E_LE     :
                        case E_GT     :
                        case E_GE     :
                        case E_EQ     :
                        case E_NE     :
                        case E_AND    :
                        case E_OR     : printbinop(logbinopname[ep->operator-E_LT],
                                                         ep);
                                        break;

                        case E_NOT    : printunop("E_NOT", ep);
                                        break;
                        case E_NEG    : 
                                        printunop("E_NEG", ep);
                                        break;

                        case E_ABS    :
                        case E_ATAN   :
                        case E_COS    :
                        case E_EXP    :
                        case E_LOG    :
                        case E_LOG10  :
                        case E_NINT   :
                        case E_SIN    :
                        case E_SQRT   :
                        case E_TAN    :
                        case E_TOREAL :
                        case E_ASIN   :
                        case E_ACOS   :
                        case E_SINH   :
                        case E_COSH   :
                        case E_TANH   : 
                        case E_CUMNOR :
                        case E_INVNOR :
                                     printfargs(btname[ep->operator-E_LOG],
                                                        ep);
                                        break;
                        case E_HYPOT  :
                        case E_FIBUR  :
                                     printfargs(btname2[ep->operator-E_HYPOT],
                                                        ep);
                                        break;
                        case E_MIN    :
                        case E_MAX    :
                                       printfargs(btname3[ep->operator-E_MAX],
                                                        ep);
                                        break;

                        case E_GOTO   : xprintf( "goto" ); break;

                        case E_STOP   : xprintf( "stop" ); break;
                        case E_START  : xprintf( "start"); break;
                        case E_BAD    : xprintf( "EBAD "); break;

                        default : xprintf("Invalid opcode %d", ep->operator);
                                  break;
                }
                xprintf("\n");
        }

        xprintf("E_STOP\n");
}

static void    print_ecode( FILE *fp, Symbol *sp )
{
        size_t  i;

        setfpout(fp);

        if( sp->xpctype == XP_EQN )
        {
                Equation *eqnp = sp->u.eqnp;

                xprintf( "Equation=%s lhs=%s codestart=%u\n",
                                sp->name, eqnp->lhs->name, eqnp->estart);
                print_enodes(eqnp->ecp);
        }

        else if( sp->xpctype == XP_FUNC )
        {
                Funcdef *fnp = sp->u.funp;

                xprintf( "Function %s codestart=%u\n", sp->name, fnp->estart);

                for( i = 0; i < fnp->argcnt ; i++)
                    if (fnp->argdp[i]->atype == ARG_NOTUSE)
                         xprintf( "Arg %u name=%s atype=%d\n",
                                        i,
                                        fnp->argnames[i],
                                        fnp->argdp[i]->atype);
                    else 
                        xprintf( "Arg %u name=%s atype=%d "
                                 "lower=%d lower_del =%d upper=%d\n",
                                        i,
                                        fnp->argnames[i],
                                        fnp->argdp[i]->atype,
                                        fnp->argdp[i]->lower,
                                        fnp->argdp[i]->lower_del,
                                        fnp->argdp[i]->upper);

                print_enodes(fnp->fcp);
        }
}

/*
 * internal ecode output
 */

void out_ecode(FILE *fp)
{
    size_t i;

    for( i=0; i < ecnt; i++)
        print_ecode(fp, eqnp[i]);
}
