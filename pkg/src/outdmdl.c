/*
 * Output a model in Dynare model code format
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
#include "string.h"


static int sumval; /* current value of sum variable */

static  void out_simplev(char *name, int lagtype, int offset) {
    if (lagtype == 0) {
        oprintf("%s", name);
    } else if (lagtype == 1) {
        oprintf( "%s(%d)", name, offset);
    } else if (lagtype == 2) {
        int k = sumval + offset;
        k = sumval + offset;
        if (k) {
            oprintf( "%s(%d)", name, k);
        } else {
            oprintf( "%s", name);
        }
    }
}

//  Dynare does not support parameter indexing, so parameters are always scalars
static void out_simplepar(Symbol *sp, int lagtype, int offset) {
    char    *name = sp->name;
    oprintf("%s", name);
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
            opname = "=="; break;
        case E_NE :
            opname = "!="; break;
        case E_AND :
            opname = "and"; break;
        case E_OR :
            opname = "or"; break;
        case E_NOT :
            opname = "not"; break;
        default:
            opname = "?";
     }
    return opname;
}

static  void out_arglist( Enode *ebase, Enode *ep ) {
    /* 
     * Print list of actual arguments in a user function call
     */
    out_arglistbuiltin(ebase, ep);
}

static void out_call( Enode *ebase, Enode *ep) {
    if (subst) {
        out_call_subst(ebase, ep);
    } else { 
        oprintf( "%s(" , ep->first.sp->name );
        out_arglist(ebase, ep);
        oprintf( ")" );
    }
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
                        // TODO: parentheses may be needed if the expression
                        // in the summation contains an operator with lower
                        // precedence than +.
                        sumsp = ep->first.sp;
                        oprintf("(");
                        for( sumval =  sumsp->u.sumvarp->low;
                             sumval <= sumsp->u.sumvarp->high;
                             sumval++) {
                            out_enode(ebase, ep->second.ep);
                            if(sumval != sumsp->u.sumvarp->high) {
                                oprintf(" + ");
                            }
                        }
                        oprintf( ")" );
                        break;


        case E_DEL    : /* del(..) */
                        oprintf( "Del(%d : ", ep->first.offset);
                        out_enode(ebase, ep->second.ep);
                        oprintf( " )" );
                        break;

        case E_IF     : /* condition then else */
                        out_oper(E_MUL, ebase, ep->first.ep, 1);
                        oprintf( " *  " );
                        out_oper(E_MUL, ebase, ep->second.ep, 0);
                        oprintf( " + ((1 -  " );
                        out_oper(E_SUB, ebase, ep->first.ep, 1);
                        oprintf( ") *  " );
                        out_oper(E_MUL, ebase, ep->third.ep, 0);
                        oprintf(")");
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
                        out_oper(ep->operator, ebase, ep->first.ep, 1);
                        oprintf( " %s ", get_opname(ep->operator));
                        out_oper(ep->operator, ebase, ep->second.ep, 0);
                        break;
        case E_OR     :
                        oprintf("((");
                        out_oper(E_ADD, ebase, ep->first.ep, 1);
                        oprintf( " + ");
                        out_oper(E_ADD, ebase, ep->second.ep, 0);
                        oprintf(") > 0)");
                        break;
        case E_AND    :
                        out_oper(E_MUL, ebase, ep->first.ep, 1);
                        oprintf( " * ");
                        out_oper(E_MUL, ebase, ep->second.ep, 0);
                        break;
                /* unary */
        case E_NOT    :
                        oprintf( "(");
                        out_oper(E_EQ, ebase, ep->first.ep, 1);
                        oprintf( " == 0)");
                        break;
        case E_NEG    :
                        oprintf( " %s ", get_opname(ep->operator));
                        out_oper(ep->operator, ebase, ep->first.ep, 1);
                        break;

                /* builtin funcs 1 arg */

        case E_TOREAL :
                        oprintf("(");
                        out_arglistbuiltin(ebase, ep);
                        oprintf(")");
                        break;

        case E_ABS    : oprintf( "abs(");
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

        case E_NINT   : oprintf( "nint(", ep->first.sp->name );
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

static void out_equat(FILE *fp, Symbol *sp) {

    setfpout(fp);

    if (sp->xpctype == XP_EQN) {
        Equation *eqnp = sp->u.eqnp;
        int     eqimplicit = Is_Impleqn(eqnp);

        /*
         * put LHS_ca at start of frml equation
         * to avoid possible problems with trailing if expression
         */

        if (eqimplicit) {
            if (Is_Frmleqn(eqnp)) {
                oprintf( "%s: 0 = %s_ca + ", sp->name,eqnp->lhs->name);
            } else {
                oprintf( "%s: 0 = ", sp->name);
            }
        } else if (Is_Frmleqn(eqnp)) {
            oprintf("%s = ", eqnp->lhs->name);
        } else {
            oprintf("%s = ", eqnp->lhs->name);
        }
        setoleader();
        out_enode(eqnp->ecp, eqnp->estart);
        if (Is_Frmleqn(eqnp)) {
            oprintf(" + %s_ca", eqnp->lhs->name);
        }
        oprintf( ";\n\n" );
    }
}

static void out_func(FILE *fp, Symbol *sp) {

    setfpout(fp);

    if (sp->xpctype == XP_FUNC) {
        Funcdef *fnp = sp->u.funp;
        oprintf("external_function(name = %s, nargs = %d, first_deriv_provided);\n",
                sp->name, fnp->argcnt);
    }
}

static void out_dynpar(FILE *fp, Symbol *sp) {
    Param *p = sp->u.parp;

    /*
     * use xprintf and not oprintf
     * so that data are on one line
     */

    setfpout(fp);

    if (p->cnt == 1 )
        xprintf("%s = %.15g;", sp->name, p->u.dval);
    else {
        mcerror("Dynare cannot handle vector parameters");
    }
}

static void xprnlsp(size_t k, int nindent) {
    if (k > 1 && k % 5 == 1) {
        xprintf("\n%*s", nindent, " ");
    }
}

void out_dmdl(FILE *fp, int subst_in) {
    size_t i, k;

    subst = subst_in;

    mkvarlist();

    setfpout(fp);

    
    // endogenous variables
    xprintf("var");
    k = 1;
    for (i = 0; i < vcnt; i++) {
        if (varp[i]->xpctype == XP_ENDO) {
            xprnlsp(k, 3);
            xprintf(" %*s", -maxvnamelen, varp[i]->name);
            ++k;
        }
    }
    xprintf(";\n\n");

    // exogenous variables
    xprintf("varexo");
    k = 1;
    for (i = 0; i < vcnt; i++) {
        if (varp[i]->xpctype == XP_EXO) {
            xprnlsp(k, 6);
            xprintf(" %*s", -maxvnamelen, varp[i]->name);
            ++k;
        }
    }
    xprintf(";\n\n");

    // constant adjustments
    k = 1;
    int first = 1;
    for (i = 0; i < ecnt; i++) {
        Symbol *sp = eqnp[i];
        if (sp->xpctype != XP_EQN) {
            continue;
        }
        Equation *eqnp = sp->u.eqnp;
        if (Is_Frmleqn(eqnp)) {
            if (first) {
                xprintf("\n\n%% fit instruments\n");
                xprintf("%%$fit$\n");
                xprintf("varexo");
            }
            first = 0;
            xprnlsp(k, 6);
            int nmlen = strlen(eqnp->lhs->name);
            char *ca_name = emalloc(nmlen + 3 + 1);
            strcpy(ca_name, eqnp->lhs->name);
            strcat(ca_name, "_ca");
            xprintf(" %*s", -(maxvnamelen + 3), ca_name);
            k = k + 1;
        }
    }
    if (k > 1) xprintf(";\n%%$endfit$\n\n");


    // parameters
    if (pcnt) {
        // parameter declarations
        xprintf("parameters");
        for (i = 0; i < pcnt; i++) {
            xprnlsp(i + 1, 10);
            xprintf(" %*s", -maxpnamelen, parp[i]->name);
        }
        xprintf(";\n\n");
        // parameter values
        for (i = 0; i < pcnt; i++){
            if (i > 1 && i % 5 == 1) {
                xprintf("\n");
            }
            out_dynpar(fp, parp[i]);
            if (i < pcnt) {
                xprintf(" ");
            }
        }
        xprintf("\n\n");
    }

    // external function
    if (!subst) {
        for (i = 0; i < ecnt; i++) {
            out_func(fp, eqnp[i]);
        }
    }


    xprintf("\nmodel;\n");

    for (i = 0; i < ecnt; i++) {
        out_equat(fp, eqnp[i]);
    }
    xprintf("end;\n");
}
