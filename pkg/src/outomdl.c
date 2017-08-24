/*
 * Output a model in Isis model code format, with or without substitution
 * of user functions.
 *
 * walk through an enode tree
 * analyze
 * print in inorder with proper placing of ()
 */
#include <string.h>

#include "outovtmdl.h"
#include "outotmdl.h"

static  Funcdef *funp;  /* for printing argument names */

static  void out_enode( Enode *, Enodep );

static  void out_simplev(char *name, int lagtype, int offset)
{
    if( lagtype == 0 )
        oprintf("%s", name);
    else if( lagtype == 1 )
        oprintf( "%s[%d]", name, offset);
    else if( lagtype == 2 )
    {
        if( offset )
            oprintf( "%s[%s%+d]", name, sumsp->name, offset);
        else
            oprintf( "%s[%s]"   , name, sumsp->name);
    }
}

static void out_arg(Enode *ep) 

    /* 
     * Output a formal argument of a user function
     */
{
    if (subst) {
        out_arg_subst(ep);
    } else {
       out_var(funp->argnames[ep->first.ep], ep);
    }
}

static  void out_arglist( Enode *ebase, Enode *ep )
{
    /* 
     * Print list of actual arguments in a user function call
     */
    out_arglistbuiltin(ebase, ep);
}

static  void out_call( Enode *ebase, Enode *ep )
{
    if (subst) {
        out_call_subst(ebase, ep);
    } else { 
        oprintf( "%s(" , ep->first.sp->name );
        out_arglist(ebase, ep);
        oprintf( ")" );
    }
}

static void out_if(Enode *ebase, Enode *ep) {

    /* the if .. then ... part */
    oprintf( "if " );
    out_enode(ebase, ep->first.ep);
    oprintf( " then " );
    out_enode(ebase, ep->second.ep);
    
    Enodep else_nodep = ep->third.ep;
    Enode *else_node = ebase + else_nodep;

    /* print elseif statements */
    while (else_node->operator == E_IF) {
        oprintf( " elseif " );
        out_enode(ebase, else_node->first.ep);
        oprintf( " then " );
        out_enode(ebase, else_node->second.ep);
        else_nodep = else_node->third.ep;
        else_node = ebase + else_nodep;
    }

    /* print else part */
    oprintf( " else " );
    out_enode(ebase, else_nodep);
    oprintf( " endif" );
}


static int get_oprec(int opcode) 
{
    /*
     * Returns the precedence of the operator 
     */
    int oprec;

    switch(opcode) {
        case E_ADD : case E_SUB :
            oprec = 4;
            break;
        case E_MUL : case E_DIV :
            oprec = 5;
            break;
        case E_POW : case E_NEG :
            oprec = 6;
            break;
        case E_AND : case E_OR :
            oprec = 1;
            break;
        case E_NOT :
            oprec = 2;
            break;
        default:
            oprec = 3;
            break;
     }

    return oprec;
}

static char *get_opname(int opcode) 
{
    /*
     * Returns the operator name
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
            opname = "="; break;
        case E_NE :
            opname = "^="; break;
        case E_AND :
            opname = ".and."; break;
        case E_OR :
            opname = ".or."; break;
        case E_NOT :
            opname = ".not."; break;
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
        case E_PARAM  : out_var(ep->first.sp->name, ep); break;

        case E_ARG    : out_arg(ep);
                        break;

        case E_SUM    : /* sum(..) */
                        sumsp = ep->first.sp;
                        oprintf( "sum(%s = %d, %d : ",
                                sumsp->name,
                                sumsp->u.sumvarp->low,
                                sumsp->u.sumvarp->high);
                        out_enode(ebase, ep->second.ep);
                        oprintf( " )" );
                        break;

        case E_DEL    : /* del(..) */
                        oprintf( "del(%d : ", ep->first.offset);
                        out_enode(ebase, ep->second.ep);
                        oprintf( " )" );
                        break;

        case E_IF     : /* condition then else */
                        out_if(ebase, ep);
                        break;

        case E_CALL   : /* call user function */
                        out_call(ebase, ep);
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
        case E_ASIN   :
        case E_ACOS   :
        case E_SINH   :
        case E_COSH   :
        case E_TANH   :
        case E_TOREAL :
        case E_CUMNOR :
        case E_INVNOR :
        case E_HYPOT  :
        case E_FIBUR  :
        case E_MIN    :
        case E_MAX    :
                        oprintf( "%s(", ep->first.sp->name);
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
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

static void out_mcode( FILE *fp, Symbol *sp )
{
    size_t  i;

    setfpout(fp);

    if( sp->xpctype == XP_EQN )
    {
        Equation *eqnp = sp->u.eqnp;
        char     *eqid = Is_Frmleqn(eqnp) ? "frml " : "ident";
        int     eqimplicit = Is_Impleqn(eqnp);
        int     need_eqname = strcmp(sp->name, eqnp->lhs->name) != 0;
            
        if (need_eqname) {
            if( eqimplicit )
                oprintf( "%s %s 0(%s) = ", eqid,
                            sp->name, eqnp->lhs->name);
            else
                oprintf( "%s %s %s = ", eqid,
                            sp->name, eqnp->lhs->name);
        } else {
            if( eqimplicit )
                oprintf( "%s 0(%s) = ", eqid,
                            eqnp->lhs->name);
            else
                oprintf( "%s %s = ", eqid,
                            eqnp->lhs->name);

        }
        setoleader();
        out_enode(eqnp->ecp, eqnp->estart);
        oprintf( ";\n" );
    }

    else if(!subst && sp->xpctype == XP_FUNC )
    {
        Funcdef *fnp = sp->u.funp;

        funp = fnp;

        char *type_name = fnp->flags.is_ulfunc ? "ul_function" : "function";

        oprintf( "%s %s(" , type_name, sp->name);
       
        for( i = 0; i < fnp->argcnt ; i++)
        {
            oprintf( "%s", fnp->argnames[i] );
            if( fnp->argcnt && i < fnp->argcnt - 1)
                    oprintf( ", ");
        }

        if (!fnp->flags.is_ulfunc) {
            oprintf( ") = " );
            setoleader();
            out_enode(fnp->fcp, fnp->estart);
        } else {
            oprintf( ")");
        }
        oprintf( ";\n" );
    }
}

static void out_upar(FILE *fp, Symbol *sp)
{
    Param *p = sp->u.parp;
    size_t  i;

    setfpout(fp);

    if( p->cnt == 1 )
            oprintf("param %s %g;\n", sp->name, p->u.dval);
    else
    {
        oprintf("param %s", sp->name);
        setoleader();

        for( i = 0; i < p->cnt ; i++ )
            oprintf(" %g", p->u.dp[i] );

        oprintf(";\n");
    }
}

/*
 * model output with user defined statement functions substituted
 */

void out_omdl(FILE *fp, int subst_in)
{
    size_t i;

    subst = subst_in;

    for( i=0; i < pcnt; i++)
        out_upar(fp, parp[i]);

    for( i=0; i < ecnt; i++)
        out_mcode(fp, eqnp[i]);
}
