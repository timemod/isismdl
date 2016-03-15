/*
 * Generate inverse polish code for the equations and user functions,
 * and call Isis Fortran subroutines save_equation and save_userfunction
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "symbol.h"
#include "util.h"
#include "mcinit.h"
#include "ecode.h"
#include "ipcode.h"
#include "xpcdef.h"
#include "mchdr.h"
#include "outmdl.h"

#include "macromodtypes.h"

#define POL_STEP 100  /* the step to increase the size of array polish */

static  Symbol  *sumsp; /* for loop variable in SUM( */
static  Funcdef *funp;  /* for printing argument names */

static size_t pol_cnt = 0;
static size_t pol_size = 0;
static MCINT * polish;

/* 
 * Declaration of Fortran functions (see file src/libf7/mcisis.rf7)
 * called in  method out_ipcode
 */
FINT FNAME(save_equation)(MCINT *, MCINT *);
FINT FNAME(save_userfunction)(MCINT *, MCINT *);

static  void out_enode( Enode *, Enodep );

static inline void inpol(MCINT ip) 
{
    if (pol_cnt == pol_size - 1) {
        pol_size = pol_cnt + POL_STEP + 1;
        polish = realloc(polish, pol_size * sizeof(MCINT));
    }
    polish[pol_cnt++] = ip;
}

static  void out_varpar(Enode *ep, int argtype)
    /* 
     * Output variable, parameter or formal argument.
     * If the variable, parameter or formal argument is an actual
     * argument of a user function, then argtype is the corresponding
     * argument type of the user function. Otherwise, argtype
     * is NO_ARG.
     */
{
    MCINT   ip;
    int     lagtype;
    MCINT   offset;
    MCINT   idx; 
    Variable *vp;
    Param *p;
    Symbol *sp;

    lagtype = ep->second.offset;
    offset  = ep->third.offset;

    if (ep->operator == E_ARG) {
        ip = (argtype == ARG_REF || argtype == ARG_REFVAL) ? IP_LAF0 : IP_LIF0;
        idx = ep->first.ep; 
    } else {
        sp = ep->first.sp;
        if (ep->operator == E_MVAR) {
            ip = (argtype == ARG_REF || argtype == ARG_REFVAL) ?
                 IP_LAV0 : IP_LIV0;
            vp = sp->u.varp;
            idx = vp->var_index;
        } else if (ep->operator == E_PARAM) {
            ip = (argtype == ARG_REF) ? IP_LAP0 : IP_LIP0;
            p = sp->u.parp;
            idx = p->par_index;
        } else {
            return;
        }
    }

    /* In the model code of Isis, indices start at 1 instead of 0. */
    idx++;

    /*
     * lagtype = 0: no shift, no loopvar
     * lagtype = 1: shift, no loopvar
     * lagtype = 2: loopvar (with or without shift)
     */
    if (lagtype) {
        if (offset < 0) {
            ip += 2;
        } else if (offset > 0) {
            ip += 4;
        }
    }
    if (lagtype == 2) ip++;

    inpol(ip);
    inpol(idx);
    offset = abs(offset);
    if (offset) {
        inpol(offset);
    }
}

/* Write constant to polish code. WARNING: this code is highly
 * system-dependent.
 */
static  void out_const(double rval )
{
    union {
        int ivals[2];
        double rval;
    } u_double;

    union {
        int ival;
        float rval;
    } u_float;

    float rval_float = (float) rval;

    int ip;
    if (rval_float == rval) {
        ip = IP_LIRL;
        u_float.rval = rval_float;
        inpol(ip);
        inpol(u_float.ival);
    } else {
        ip = IP_LIRL8;
        u_double.rval = rval;
        inpol(ip);
        inpol(u_double.ivals[0]);
        inpol(u_double.ivals[1]);
    }
}


/* argument list for builtin-functions with fixed number of arguments */
static  void out_arglist( Enode *ebase, Enode *ep )
{
    size_t arglist = ep->third.ep;

    ep = ebase + arglist;

    while( arglist && ep->operator == E_ARGLIST )
    {
        out_enode(ebase, ep->second.ep);
        ep = ebase + (arglist = ep->first.ep);
    }
}

/* min and max functions (variable number of arguments). 
 * Example: polish code for max(2, 3, 4) is 2 3 MAX 4 MAX
 */
static void out_min_max(Enode *ebase, Enode *ep, int ip) 
{
    size_t arglist = ep->third.ep;
    int argCount = 0;

    ep = ebase + arglist;
    while( arglist && ep->operator == E_ARGLIST )
    {
        out_enode(ebase, ep->second.ep);
        ep = ebase + (arglist = ep->first.ep);
        if (++argCount > 1) 
            inpol(ip);
    }

}

static  void out_arglist_builtin( Enode *ebase, Enode *ep, Funcdef *fp)
{
    Argdef **adp = fp->argdp;
    Argdef *argp;
    size_t arglist = ep->third.ep;
    Enode *arg_node;

    ep = ebase + arglist;

    int cnt = 0;

    while( arglist && ep->operator == E_ARGLIST )
    {
        argp = adp[cnt++];
        arg_node = ebase + ep->second.ep;
        int op = arg_node->operator;
        if (op == E_MVAR || op == E_PARAM || op == E_ARG) {
            out_varpar(arg_node, argp->atype);
        } else {
            out_enode(ebase, ep->second.ep);
        }
        ep = ebase + (arglist = ep->first.ep);
    }
}

/* call of user function */
static  void out_call( Enode *ebase, Enode *ep )
{
    Symbol *sp;
    Funcdef *f;


    sp = ep->first.sp;
    f = sp->u.funp;
    int idx = f->func_index + 1;
    
    out_arglist_builtin(ebase, ep, f);

    int ip = f->flags.is_ulfunc ? IP_ULCALL : IP_CALL;
    inpol(ip);
    inpol(idx);
    if (f->flags.is_ulfunc) {
        inpol(f->argcnt);
    }
}

static  void out_enode( Enode *ebase, Enodep estart )
{
    Enode   *ep = ebase + estart;
    int ip = ep->operator;
    int i1, i2;

    switch( ip )
    {

        case E_RCONST : out_const(ep->first.rval); 
                        break;

        case E_SVAR   : /* sum variable */
                        inpol(ip);
                        break;

        case E_MVAR   :  /* model variable */
        case E_PARAM  :  /* model parameter */
        case E_ARG    :  /* formal argument userfunction */
                        out_varpar(ep, NO_ARG);
                        break;


        case E_SUM    : /* sum(..) */
                        sumsp = ep->first.sp;
                        inpol(ip);
                        i1 = sumsp->u.sumvarp->low;
                        i2 = sumsp->u.sumvarp->high;
                        inpol(i1);
                        inpol(i2);
                        out_enode(ebase, ep->second.ep);
                        i1 = IP_ENDSUM;
                        inpol(i1);

                        /* free the sum variable */
                        efree(sumsp->u.sumvarp);
                        efree(sumsp);

                        break;

        case E_DEL    : /* del(..) */
                        inpol(ip);
                        i1 = ep->first.offset;
                        inpol(i1);
                        out_enode(ebase, ep->second.ep);
                        i1 = IP_ENDDEL;
                        inpol(i1);
                        break;

        case E_IF     : /* if condition then else */
                        out_enode(ebase, ep->first.ep);
                        inpol(ip);
                        out_enode(ebase, ep->second.ep);
                        i1 = IP_ELSE;
                        inpol(i1);
                        out_enode(ebase, ep->third.ep);
                        i1 = IP_ENDIF;
                        inpol(i1);
                        break;

        case E_CALL   : /* call user function */
                        out_call(ebase, ep);
                        break;

        case E_ARGLIST: /* ???? */
                        /* arglist operator for ufuncs
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
                        out_enode(ebase, ep->first.ep);
                        out_enode(ebase, ep->second.ep);
                        inpol(ip);
                        break;

                /* unary */
        case E_NOT    :
        case E_NEG    :
                        out_enode(ebase, ep->first.ep);
                        inpol(ip);
                        break;

                /* built-in funcs 1 arg */
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
                        out_arglist(ebase, ep);
                        inpol(ip);
                        break;

        case E_MIN    :
        case E_MAX    :
                        out_min_max(ebase, ep, ip);
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

void    out_ipcode(Symbol *sp )
{
    int ip;
    int error = 0;
    MCINT np;

    if (polish == NULL)  {
        polish = malloc(POL_STEP * sizeof(int));
        pol_size = POL_STEP;
    }

    if( sp->xpctype == XP_EQN )
    {
        Equation *eqnp = sp->u.eqnp;
        out_enode(eqnp->ecp, eqnp->estart);
        ip = IP_STOP;
        inpol(ip);
        np = (MCINT) pol_cnt;
        error = (int) FNAME(save_equation)(&np, (MCINT *) polish);
        pol_cnt = 0;
    }
    else if( sp->xpctype == XP_FUNC )
    {
        Funcdef *fnp = sp->u.funp;
        funp = fnp;
        out_enode(fnp->fcp, fnp->estart);
        ip = IP_RET;
        inpol(ip);
        np = (MCINT) pol_cnt;
        error = (FINT) FNAME(save_userfunction)(&np, (MCINT *) polish);
        pol_cnt = 0;
    }

    /* if error (polish code too long: error message  */
    if (error) 
        mcerror("Formula too long for processing");
}

void free_polish(void) 
    /* free the memory used to generate polish code */
{
    efree(polish);
    polish = NULL;
}
