/*
 * Output a model in EViews model code format
 *
 * walk through an enode tree
 * analyze
 * print in inorder with proper placing of ()
 *
 * and substitute user function calls
 *
 */

#include <string.h>
#include "outovtmdl.h"
#include "outvtmdl.h"
#include "isismdl.h"

static int sumval; /* current value of sum variable */

static char *get_opname(int opcode) 
{
    /*
     * Returns the operator name for EViews models
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
            opname = "^"; break;
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

/*
 * EViews: parameter index with positive values
 */

static  void out_simplepar(Symbol *sp, int lagtype, int offset)
{
    char    *name = sp->name;
    int     k;

    if( lagtype == 0 )
        oprintf("%s(1)", name);
    else if( lagtype == 1 )
        oprintf( "%s(%d)", name, 1-offset);
    else if( lagtype == 2 )
    {
        k = 1 - (sumval + offset);
        oprintf( "%s(%d)", name, k);
    }
}

/*
 * variables only
 */

static  void out_simplev(char *name, int lagtype, int offset) {
    int k;

    if (strlen(name) > 24) {
        fprintf(stderr, "Rhs variable %s" \
                " too long for EViews (max = 24)\n", name);
    }

    if( lagtype == 0 ) 
        oprintf("%s", name);
    else if( lagtype == 1 )
        oprintf( "%s(%d)", name, offset);
    else if( lagtype == 2 )
    {
        k = sumval + offset;
        if(k)
            oprintf( "%s(%d)", name, k);
        else
            oprintf( "%s", name);
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

static int out_min_max(Enode *ebase, Enode *ep, int is_max) {

    /* output the min or max function. EViews does not have
     * an min and max function.  Therefore the @recode function
     * is used. The current implementation only works if min/max
     * has two argumentsk. */

    size_t arglist = ep->third.ep;
    Enode *ep1 = ebase + arglist;
    Enode *ep2 = ebase + (arglist = ep1->first.ep);
    if (ep2->first.ep) {
        return 1;
    }

    oprintf( "@recode(");
    out_enode(ebase, ep1->second.ep);
    if (is_max) {
        oprintf(" > ");
    } else {
        oprintf(" < ");
    }
    out_enode(ebase, ep2->second.ep);
    oprintf( ", " );
    out_enode(ebase, ep1->second.ep);
    oprintf( ", " );
    out_enode(ebase, ep2->second.ep);
    oprintf(")");

    return 0;
}


static  void out_enode( Enode *ebase, Enodep estart )
{
    Enode   *ep = ebase + estart;
    Symbol *sumsp;

    switch( ep->operator )
    {

        case E_RCONST : out_const(ep->first.rval); break;

        case E_SVAR   : out_const(sumval); break;
        case E_MVAR   : out_var(ep->first.sp->name, ep); break;
        case E_PARAM  : out_par(ep->first.sp, ep); break;

        case E_ARG    : out_arg_subst(ep);
                        break;

        case E_SUM    : /* sum(..) */
                        sumsp = ep->first.sp;
                        oprintf("(");
                        for( sumval =  sumsp->u.sumvarp->low;
                             sumval <= sumsp->u.sumvarp->high;
                             sumval++)
                        {
                            out_enode(ebase, ep->second.ep);
                            if(sumval != sumsp->u.sumvarp->high)
                                oprintf(" + ");
                        }
                        oprintf( ")" );
                        break;

        case E_DEL    : /* del(..) */
                        oprintf( "d(");
                        out_enode(ebase, ep->second.ep);

                        if( ep->first.offset > 1 )
                            oprintf( ",%d", ep->first.offset);
                        oprintf( ")" );
                        break;

        case E_IF     : /* condition then else */
                        oprintf( "@recode( " );
                        out_enode(ebase, ep->first.ep);     /* condition */
                        oprintf( ", " );
                        out_enode(ebase, ep->second.ep);    /* then expression */
                        oprintf( ", " );
                        out_enode(ebase, ep->third.ep);     /* else expression */
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
        case E_NOT    : ERROR("EViews does not support a logical not operator\n");
                        break;
        case E_NEG    :
                        oprintf( " %s ", get_opname(ep->operator));
                        out_oper(ep->operator, ebase, ep->first.ep, 1);
                        break;

                /* builtin funcs 1 arg */

        case E_TOREAL :
                        oprintf( "(" );
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;

        case E_ABS    : oprintf( "abs(");
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;
        case E_ATAN   :
        case E_ASIN   :
        case E_ACOS   :
        case E_COS    :
        case E_SIN    :
        case E_TAN    :
                        oprintf( "@%s(", ep->first.sp->name );
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;

        case E_EXP    :
        case E_LOG    :
                        oprintf( "%s(", ep->first.sp->name );
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;

        case E_SQRT   :
                        oprintf( "sqr(");
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;

        case E_LOG10  : oprintf("(log(");
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")/log(10))" );
                        break;

        case E_HYPOT  : out_hypot(ebase, ep);
                        fprintf(stderr, "%s \n", "Builtin function hypot replaced by explicit expression");
                        break;

        case E_FIBUR  : out_fibur(ebase, ep);
                        fprintf(stderr, "%s \n", "Builtin function fibur replaced by explicit expression");
                        break;

        case E_SINH   :
        case E_COSH   :
        case E_TANH   : 
                        ERROR("EViews does not support hyperbolic functions\n");
                        break;

        case E_CUMNOR   :
        case E_INVNOR   :
                        ERROR("EViews does not support the functions cumnor and invnor\n");
                        break;


        case E_NINT   : oprintf( "@round(", ep->first.sp->name );
                        out_arglistbuiltin(ebase, ep);
                        oprintf( ")" );
                        break;

                /* builtin with 2 or more args */
        case E_MIN    :
        case E_MAX    : 
                        if (out_min_max(ebase, ep, ep->operator == E_MAX)) {
                            ERROR("Conversion to EViews cannot handle "
                                  "min and max with more than two arguments.\n");
                        }
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

static void    out_eviewscode( FILE *fp, Symbol *sp, char *mdlname )
{
    setfpout(fp);

    if (sp->xpctype == XP_EQN) {

        Equation *eqnp = sp->u.eqnp;
        int     eqimplicit = Is_Impleqn(eqnp);

        oprintf("%s.append ", mdlname);

        if (strlen(eqnp->lhs->name) > 22) {
            fprintf(stderr, "Lhs variable %s" \
                    " too long for EViews (max = 22)\n", eqnp->lhs->name);
        }

        /*
         * put LHS_ca at start of frml equation
         */

        if (eqimplicit) {
            oprintf( "0 = ");
        } else {
            oprintf( "%s = ", eqnp->lhs->name);
        }

        setoleader();
        out_enode(eqnp->ecp, eqnp->estart);
        oprintf( "\n" );

        if (Is_Frmleqn(eqnp)) {
            oprintf( "%s.addassign %s\n", mdlname, eqnp->lhs->name);
            oprintf( "%s.addinit(v=z) %s\n", mdlname, eqnp->lhs->name);
        }
    }
}

static void out_eviewspar(FILE *fp, Symbol *sp)
{
    Param *p = sp->u.parp;
    size_t  i;

    /*
     * use xprintf and not oprintf
     * so that data are on one line
     */

    setfpout(fp);

    if( p->cnt == 1 )
            xprintf("coef %s=%g\n", sp->name, p->u.dval);
    else
    {
        xprintf("coef(%d) %s\n", p->cnt, sp->name);

        xprintf("param ");

        for( i = 0; i < p->cnt ; i++ )
            xprintf("%s(%d) %g ", sp->name, i+1, p->u.dp[i] );

        xprintf("\n");
    }
}

/*
 * output EViews model code in file to be included
 */

void out_vmdl(FILE *fp, char *optmdlname)
{
    size_t i;
    char *mdlname;

    /* use mdl as default modelname */

    mdlname = (optmdlname == NULL) ? "mdl" : optmdlname;

    setfpout(fp);

    setlinecont('_');

    for( i=0; i < pcnt; i++)
            out_eviewspar(fp, parp[i]);

    oprintf("model %s\n", mdlname);

    for( i=0; i < ecnt; i++)
        out_eviewscode(fp, eqnp[i], mdlname);
}
