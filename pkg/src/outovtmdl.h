/*
 * Common variables and functions for model output 
 * for Isis, Eviews and Troll
 * (outomdl.c, outvmdl.c and outtmdl.c)
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
#include "outmdl.h"
#include "oprintf.h"
#include "isismdl.h"

#define MAXSTACK 500

/* if subst = 1, then user functions are substituted */
static int subst =  1;

static void out_simplev(char *name, int lagtype, int offset);
static int get_oprec(int opcode);
static Enode* subst_formal_arg(Enode *ep, int *lagtype, int *offset);

/*
 * to keep track of expressions passed as argument to user functions
 */

typedef struct _selem {
    Enodep *arguments;
    Enode  *ebase;
    size_t argbase_prev; /* address on stack of calling function */
}
arg_stackelem;

static  arg_stackelem arg_stack[MAXSTACK];

static  size_t  stackp  = 0; /* next free slot in stack */
static  size_t  argbase = -1; /* start of argument in current call frame */

static  void out_enode( Enode *, Enodep );

static  void out_var( char *name, Enode *ep )
{
    int     lagtype;
    int     offset;

    if( ep )
    {
        lagtype = ep->second.offset;
        offset  = ep->third.offset;
    }
    else
        lagtype = 0;

    out_simplev(name, lagtype, offset);
}

static  void out_arg_subst(Enode *ep)
{
    /*
     * Output a formal argument of a user function by
     * substitution: the enode tree of the actual
     * argument is printed.
     */ 

    /*
    * lagtype + offset of argument in function code
    * if required apply to argument
    */

    int  arglagtype = ep->second.offset;

    if (arglagtype) {
    
        int lagtype, offset;
        Enode *vp = subst_formal_arg(ep, &lagtype, &offset);

        if (vp->operator != E_MVAR) {  
            /* this error should not occur */
            ERROR("Expression in function argument not allowed here\n");
        }

        /*
         * add in lag/lead of variable in function call
         */

        out_simplev(vp->first.sp->name, lagtype, offset);

    } else  {

        size_t argnum = ep->first.ep;
        Enodep argp  = arg_stack[argbase].arguments[argnum];
        Enode *ebase = arg_stack[argbase].ebase;
        size_t argbase_sav = argbase;

        argbase = arg_stack[argbase].argbase_prev;
        out_enode(ebase, argp);
        argbase = argbase_sav;
   }
}

static  void out_const( real rval )
{
    oprintf( "%.15g", rval);
}

static  void out_arglistbuiltin( Enode *ebase, Enode *ep )
{
    size_t arglist = ep->third.ep;
    ep = ebase + arglist;
    while( arglist && ep->operator == E_ARGLIST )
    {
            out_enode(ebase, ep->second.ep);
            ep = ebase + (arglist = ep->first.ep);
            if( arglist )
                    oprintf( ", " );
    }
}

static void push_arglist( Enode *ebase, Enode *ep, int argcnt)
{
    /*
     * push argument expressions on stack
     */

    Enode *enext;
    size_t arglist = ep->third.ep;
    int cnt;

    if (stackp >= MAXSTACK) {
        ERROR("arg_stack too large\n");
    }

    Enodep *arguments = emalloc(argcnt * sizeof(Enodep));

    enext = ebase + arglist;
    cnt = 0;
    while (arglist && enext->operator == E_ARGLIST ) {
        arguments[cnt++]  = enext->second.ep;
        enext = ebase + (arglist  = enext->first.ep);
    }

    arg_stack[stackp].arguments = arguments;
    arg_stack[stackp].ebase = ebase;
    arg_stack[stackp].argbase_prev = argbase;

    argbase = stackp++;
}

static void pop_arglist(void) 
{
    free(arg_stack[argbase].arguments);
    argbase--;
    stackp--;
}

static  void out_call_subst(Enode *ebase, Enode *ep )
{
    /*
     * Output of a user function call by substitution
     */

    Funcdef *fnp = ep->first.sp->u.funp;
    push_arglist(ebase, ep, fnp->argcnt);
    out_enode(fnp->fcp, fnp->estart);
    pop_arglist();
}

static int is_binary(int opcode) 
{
    /*
     * Returns 1 if opcode is a binary operator, and 0 otherwise
     */
    return opcode >= E_ADD && opcode <= E_AND
           && opcode != E_MAX && opcode != E_MIN;
}

static int is_unary(int opcode) 
{
    /*
     * Returns 1 if opcode is a unary operator (E_NEG or E_NOT),
     * and 0 otherwise
     */
    return opcode == E_NEG || opcode == E_NOT;
}

static  int oprecgt( int opcode, Enode *ebase, Enodep nextp , int isleft)
{
    /*
     * isleft <> 0 then nextp is left node
     *              and no special tests needed
     *
     * if nextp is the right node and
     * opcode and nextop have equal precedence
     * special tests needed to output nasty expressions such as
     *
     *    a ** b ** c    !! not clear will be output as next below
     *    a **(b ** c)
     *    (a ** b) ** c
     *    x-(p+q)
     *    x-(p-q)
     *    x/(p/q)
     *    x/(p*q)
     *
     * correctly
     * for the ** operator redundant () are generated for clarity
     */

    int nextop;
    Enode *nexte = ebase + nextp;
    int idum1, idum2; 
    if (subst) {
        nexte = subst_formal_arg(nexte, &idum1, &idum2);
    }
    nextop = nexte->operator;

    if (subst && nextop == E_CALL) {
    
        /*
         * replace call by first operator of function body
         * and continue precedence checking
         */

        Funcdef *fnp;
        Enode   *fep;

        while ((nextop = nexte->operator) == E_CALL) {
            fnp = nexte->first.sp->u.funp;
            fep = fnp->fcp + fnp->estart;
            nexte = fep;
        }
    
        if (nextop == E_ARG) {
            /* force ( ). The () may be superfluous, but is hard 
             * to decide whether the () are necessary or not */
            return 1;
        }
    }
    

    if( nextop == E_IF )    /* if has lowest precedence in modelling language */
        return 1;

    if (!(is_unary(nextop) || is_binary(nextop))) {
        return 0;
    }

    if (get_oprec(opcode) > get_oprec(nextop))
        return 1;
    else if( get_oprec(opcode) < get_oprec(nextop))
        return 0;
    else if( opcode == E_POW )
        /*
         * right associative
         * here 1 is returned to always generate ()
         * if that is not needed then change to
         *      return isleft;
         */
        return 1;
    else if( opcode == E_SUB || opcode == E_DIV )
        return !isleft;
    else
        return 0;
}

static Enode *subst_formal_arg(Enode *ep, int *lagtype, int *offset)
{
    /*
     * Substitute formal arguments of user functions by actual arguments
     * until it finds an actual argument that is not a formal argument.
     * The final enode returned can be anything except a formal
     * argument: it may be an expression containing formal arguments.
     */

     size_t argnum;
     Enodep argp;
     Enode *ebase;
     size_t argbase_tmp = argbase;

     *lagtype   = ep->second.offset;
     *offset    = ep->third.offset;

     while (ep->operator == E_ARG) {
         argnum      = ep->first.ep;
         argp        = arg_stack[argbase_tmp].arguments[argnum];
         ebase       = arg_stack[argbase_tmp].ebase;
         ep          = ebase + argp;
         if (ep->second.offset) {
             *lagtype = max(*lagtype, ep->second.offset);
             *offset += ep->third.offset;
         }
         argbase_tmp = arg_stack[argbase_tmp].argbase_prev;
     }
     return ep;
}

static  void out_oper( int opcode, Enode *ebase, Enodep nextp , int isleft)
{
    if( oprecgt(opcode, ebase, nextp, isleft) )
    {
        oprintf( "(" );
        out_enode(ebase, nextp);
        oprintf( ")" );
    }
    else
        out_enode(ebase, nextp);
}
