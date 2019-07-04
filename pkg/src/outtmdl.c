/*
 * Output a model in Troll model code format
 *
 * walk through an enode tree
 * analyze
 * print in inorder with proper placing of ()
 *
 * and substitute user function calls
 *
 */

#include "outovtmdl.h"
#include "outvtmdl.h"
#include "outotmdl.h"
#include "isismdl.h"

static  void out_simplev(char *name, int lagtype, int offset)
{
    if( lagtype == 0 )
        oprintf("%s", name);
    else if( lagtype == 1 )
        oprintf( "%s(%d)", name, offset);
    else if( lagtype == 2 )
    {
        if( offset )
            oprintf( "%s(%s%+d)", name, sumsp->name, offset);
        else
            oprintf( "%s(%s)"   , name, sumsp->name);
    }
}
/*
 * Troll requires parameter indexing to use []
 * in addition parameters must be index with positive values
 */

static  void out_simplepar(Symbol *sp, int lagtype, int offset)
{
    char    *name = sp->name;
    Param   *parm = sp->u.parp;

    if( lagtype == 0 )
    {
        if(parm->cnt == 1)
            oprintf("%s", name);
        else
            oprintf("%s[1]", name);
    }
    else if( lagtype == 1 )
        oprintf( "%s[%d]", name, 1-offset);
    else if( lagtype == 2 )
    {
        if( offset )
            oprintf( "%s[1-(%s%+d)]", name, sumsp->name, offset);
        else
            oprintf( "%s[1-%s]"   , name, sumsp->name);
    }
}

static  void out_hypot( Enode *ebase, Enode *ep )
{
    size_t arglist = ep->third.ep;

    ep = ebase + arglist;

    oprintf("sqr(");
    out_arg_substbuiltin(ebase, ep);
    oprintf("^2 + ");
    ep = ebase + (arglist = ep->first.ep);
    out_arg_substbuiltin(ebase, ep);
    oprintf("^2) ");
}

static char *get_opname(int opcode) 
{
    /*
     * Returns the operator name for Eviews models
     */
    char* opname;

    switch(opcode) {
        case E_ADD : 
            opname = "+"; break;
        case E_SUB :
            opname = "-"; break;
        case E_MUL : 
            opname = "*"; break;
        case E_DIV :
            opname = "/"; break;
        case E_POW :
            opname = "**"; break;
        case E_NEG :
            opname = "-"; break;
        case E_LT :
            opname = "<"; break;
        case E_LE :
            opname = "<="; break;
        case E_GT :
            opname = ">"; break;
        case E_GE :
            opname = ">="; break;
        case E_EQ :
            opname = "=="; break;
        case E_NE :
            opname = "<>"; break;
        case E_AND :
            opname = "and"; break;
        case E_OR :
            opname = "or"; break;
        case E_NOT :
            opname = "not"; break;
     }
    return opname;
}

static  void out_enode( Enode *ebase, Enodep estart )
{
    Enode   *ep = ebase + estart;

    switch( ep->operator )
    {

        case E_RCONST : out_const(ep->first.rval); break;

        case E_SVAR   : out_var(sumsp->name, NULL); break;
        case E_MVAR   : out_var(ep->first.sp->name, ep); break;
        case E_PARAM  : /*out_par(ep->first.sp->name, ep); break;*/
                        out_par(ep->first.sp, ep); break;

        case E_ARG    : out_arg_subst(ep);
                        break;

        case E_SUM    : /* sum(..) */
                        sumsp = ep->first.sp;
                        oprintf( "Sum(%s = %d to %d : ",
                                sumsp->name,
                                sumsp->u.sumvarp->low,
                                sumsp->u.sumvarp->high);
                        out_enode(ebase, ep->second.ep);
                        oprintf( " )" );
                        break;

        case E_DEL    : /* del(..) */
                        oprintf( "Del(%d : ", ep->first.offset);
                        out_enode(ebase, ep->second.ep);
                        oprintf( " )" );
                        break;

        case E_IF     : /* condition then else */
                        oprintf( "if( " );
                        out_enode(ebase, ep->first.ep);
                        oprintf( " ) then (" );
                        out_enode(ebase, ep->second.ep);
                        oprintf( ") else (" );
                        out_enode(ebase, ep->third.ep);
                        oprintf(")");
                        break;

        case E_CALL   : /* call user function */
                        out_call_subst(ebase, ep);
                        break;

        case E_ARGLIST: /* arglist operator for ufuncs
                         * and min/max
                         * format as binary
                         */

                        break;

                /*  binary */

        case E_ADD    :
        case E_SUB    :
        case E_MUL    :
        case E_DIV    :
        case E_POW    :
        case E_LT     :
        case E_LE     :
        case E_GT     :
        case E_GE     :
        case E_EQ     :
        case E_NE     :
        case E_AND    :
        case E_OR     :
                        out_oper(ep->operator, ebase, ep->first.ep, 1);
                        oprintf( " %s ", get_opname(ep->operator));
                        out_oper(ep->operator, ebase, ep->second.ep, 0);
                        break;

                /* unary */
        case E_NOT    :
        case E_NEG    :
                        oprintf( " %s ", get_opname(ep->operator));
                        out_oper(ep->operator, ebase, ep->first.ep, 1);
                        break;

                /* builtin funcs 1 arg */

        case E_TOREAL :
                        oprintf( "if(" );
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ") then (1) else (0)" );
                        break;

        case E_ABS    : oprintf( "absv(");
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;
        case E_ATAN   :
        case E_COS    :
        case E_EXP    :
        case E_LOG    :
        case E_LOG10  :
        case E_SIN    :
        case E_SQRT   :
        case E_TAN    :
        case E_ASIN   :
        case E_ACOS   :
        case E_SINH   :
        case E_COSH   :
        case E_TANH   :
                        oprintf( "%s(", ep->first.sp->name );
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;

        case E_NINT   : oprintf( "Round(", ep->first.sp->name );
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;

                /* builtin with 2 or more args */
        case E_MIN    :
        case E_MAX    :
                        oprintf( "%s(", ep->first.sp->name);
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;

        case E_CUMNOR :
        case E_INVNOR :
                        ERROR("Troll does not support the functions cumnor and invnor\n");
                        break;

        case E_HYPOT  : out_hypot(ebase, ep);
                        fprintf(stderr, "%s \n", "Builtin function hypot replaced by explicit expression");
                        break;

        case E_FIBUR  : out_fibur(ebase, ep);
                        fprintf(stderr, "%s \n", "Builtin function fibur replaced by explicit expression");
                        break;


                       

        case E_GOTO   :
        case E_STOP   :
        case E_START  : break;
        case E_BAD    : fprintf(stderr, "EBAD opcode");
                        break;

        default       : fprintf(stderr, "Invalid opcode %d",
                                ep->operator);
                        break;
    }
}

static void    out_trollcode( FILE *fp, Symbol *sp )
{
    setfpout(fp);

    if( sp->xpctype == XP_EQN )
    {
        Equation *eqnp = sp->u.eqnp;
        int     eqimplicit = Is_Impleqn(eqnp);

        /*
         * put LHS_ca at start of frml equation
         * to avoid possible problems with trailing if expression
         */

        if( eqimplicit )
        {
            if( Is_Frmleqn(eqnp) )
                oprintf( "%s: 0 = %s_ca + ", sp->name,eqnp->lhs->name);
            else
                oprintf( "%s: 0 = ", sp->name);
        }
        else if( Is_Frmleqn(eqnp) )
            oprintf( "%s: %s = %s_ca + ",
                     sp->name, eqnp->lhs->name, eqnp->lhs->name);
        else
            oprintf( "%s: %s = ",
                      sp->name, eqnp->lhs->name);
        setoleader();
        out_enode(eqnp->ecp, eqnp->estart);
        oprintf( ",\n" );
    }
}

static void out_trollpar(FILE *fp, Symbol *sp)
{
    Param *p = sp->u.parp;
    size_t  i;

    /*
     * use xprintf and not oprintf
     * so that data are on one line
     */

    setfpout(fp);

    if( p->cnt == 1 )
            xprintf("  %s = %.15g,\n", sp->name, p->u.dval);
    else
    {
        xprintf("  %s = combine(", sp->name);
        setoleader();

        for( i = 0; i < p->cnt-1 ; i++ )
            xprintf(" %.15g,", p->u.dp[i] );

        if( p->cnt-1 )
            xprintf(" %.15g)", p->u.dp[p->cnt-1] );

        xprintf(",\n");
    }
}

static void xprnlsp(size_t k, size_t kmax)
{
    if( k % 5 == 1 )
        xprintf("\n     ");
}

void out_tmdl(FILE *fp, FILE *fparam)
{
    size_t i, k;

    setfpout(fparam);

    xprintf("// Parameter data\n\n//Change do into something else if needed\n\n");
    xprintf("do\n");

    for( i=0; i < pcnt; i++)
        out_trollpar(fparam, parp[i]);

    xprintf(";\n\n");

    xprintf("//End of parameter data\n\n\n");

    setfpout(fp);

    mkvarlist();

    xprintf("addsym endogenous");

    k = 1;
    for( i=0; i < vcnt; i++)
    {
        if( varp[i]->xpctype == XP_ENDO )
        {
            xprnlsp(k, endocnt);
            xprintf("%*s ", -maxvnamelen, varp[i]->name);
            ++k;
        }
    }
    xprintf("\n,\n");

    xprintf("exogenous");

    k = 1;
    for( i=0; i < vcnt; i++)
    {
        if( varp[i]->xpctype == XP_EXO )
        {
            xprnlsp(k, exocnt);
            xprintf("%*s ", -maxvnamelen, varp[i]->name);
            ++k;
        }
    }

    if( pcnt )
    {
        xprintf("\n,\n");
        xprintf("parameter");

        for( i=0; i < pcnt; i++)
        {
            xprnlsp(i+1,pcnt);
            xprintf("%*s ", -maxpnamelen, parp[i]->name);
        }
    }

    xprintf("\n,\n");

    xprintf(";\n\n");
    xprintf("addeq bottom\n\n");

    for( i=0; i < ecnt; i++)
        out_trollcode(fp, eqnp[i]);

    xprintf(";\n");
}
