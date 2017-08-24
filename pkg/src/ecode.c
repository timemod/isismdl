#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "symbol.h"
#include "util.h"
#include "mcinit.h"
#include "ecode.h"
#include "xpcdef.h"
#include "xpctab.h"
#include "mchdr.h"
#include "dependencies.h"

#define  EMAX_INIT 100

static  size_t emax  = 0;
static  size_t curep = 0;
static  size_t epidx = 0;

static  Enode  *enbase = NULL;     /* current base of an Enode array */
static  Enode  *ep     = NULL;     /* current free element           */

/*
 * for conversion of token values to E_... opcodes
 */

static  int mk_bin_ecode  ( int );
static  int mk_un_ecode   ( int );
static  int mk_bltin_ecode( int );
static  void new_enode( void );
static  void par_error(char * parname, int lower, int upper, int par_len,
                       char *funcname);

extern Argdef  **argdefs;

void    open_enode( void )
{
        if (enbase == NULL) 
        {
            emax = EMAX_INIT;
            enbase = emalloc( emax * sizeof(Enode) );
        }
        enbase->operator = E_START;
        curep = 1;
}

Enode   *close_enode(void) {
        Enode   *rep;

        new_enode();

        ep->operator = E_STOP;

        if (options.McIsisMdl) {
            curep = 0;
            rep = enbase;
        } else {
            rep = erealloc( enbase, curep * sizeof(Enode) );
            emax  = 0;
            curep = 0;
            enbase= NULL;
        }

        return rep;
}

void    reset_enode( void )
{
    curep = 0;
}

void free_enodes(void) 
    /* free memory */
{
    efree(enbase);
    emax  = 0;
    curep = 0;
    enbase= NULL;
}

static  void new_enode( void )
{

        if( curep >= emax )
        {
                emax += 10;
                enbase = erealloc( enbase, emax * sizeof(Enode) );
        }
        epidx = curep++;
        ep    = &enbase[epidx];
}

Enodep  mk_binenode( int op, Enodep left, Enodep right)
{
        new_enode();

        ep->operator  = mk_bin_ecode(op);
        ep->first.ep  = left;
        ep->second.ep = right;

        return epidx;
}

Enodep  mk_const( real r )
{
        new_enode();

        ep->operator   = E_RCONST;
        ep->first.rval = r;

        return epidx;
}

Enodep  mk_unop( int op, Enodep expr )
{
        new_enode();

        ep->operator = mk_un_ecode(op);
        ep->first.ep = expr;

        return epidx;
}

Enodep  mk_ifnode( Enodep cond, Enodep thenpart)
{
        new_enode();

        ep->operator  = E_IF;
        ep->first.ep  = cond;
        ep->second.ep = thenpart;
        ep->third.ep  = 0;

        return epidx;
}

Enodep add_elsepart(Enodep ifnode, Enodep elsepart)
{
    /* add else part to an if tree */

    Enode *ifep;
    ifep = enbase + ifnode;

    while (ifep->third.ep && ifep->operator == E_IF) {
        ifep = enbase + ifep->third.ep;
    }

    ifep->third.ep  = elsepart;

    return ifnode;
}

Enodep  mk_del( int deloff, Enodep expr )
{
        new_enode();

        ep->operator     = E_DEL;
        ep->first.offset = deloff;
        ep->second.ep    = expr;

        return epidx;
}

Enodep  mk_sum( Symbol *sumsp, Enodep expr)
{
        new_enode();

        ep->operator  = E_SUM;
        ep->first.sp  = sumsp;
        ep->second.ep = expr;

        return epidx;
}

Enodep  mk_var( Symbol *sp , int lagtype, int lagoffset )
{
        new_enode();

        ep->first.sp = sp;

        switch( sp->xpctype )
        {
                case XP_PARAM  : ep->operator = E_PARAM;
                                 break;

                case XP_FNARG  : ep->operator = E_ARG  ;
                                 ep->first.ep = sp->u.handle;
                                 break;

                case XP_SVAR   : ep->operator = E_SVAR ;
                                 break;

                case XP_EXO    : /* FALL THROUGH */
                case XP_ENDO   : ep->operator = E_MVAR ;
                                 break;

                default        : ep->operator = E_BAD; break;
        }

        /*
         * store lagtype in second field
         *  0 no lags
         *  1 ordinary lag
         *  2 lag is sumindex + lagoffset
         */

        ep->second.offset = lagtype;
        ep->third.offset  = lagoffset;

        return epidx;
}

size_t  arglist_count( Enodep arglist )
{
        /*
         * calculate # of arguments in call of a function
         * done relative to current ecode base
         */

        Enode   *ep;
        size_t  cnt = 0;

        ep = enbase + arglist;

        while( arglist && ep->operator == E_ARGLIST )
        {
                ++cnt;
                ep = enbase + (arglist = ep->first.ep);
        }

        return cnt;
}

Enodep  mk_builtin( Symbol *sp, Enodep arglist )
{
        size_t  argcnt;

        new_enode();

        ep->operator = mk_bltin_ecode( sp->xpctype );
        ep->first.sp = sp;
        ep->second.ep = argcnt = arglist_count(arglist);
        ep->third.ep  = arglist;

        return epidx;
}

Enodep  mk_fargs( Enodep firstp, Enodep expr)
{
        /* <firstp> points to the start of a comma separated
         * list of expressions, except when 0 then <expr> is the first
         * the current expression <expr> must be tacked
         * on to the end
         * return value is the head of the list <firstp>
         */

        Enode   *cep;

        new_enode();
        ep->operator  = E_ARGLIST;
        ep->first.ep  = 0;
        ep->second.ep = expr;

        if( firstp == 0 )
                firstp = epidx;  /* current just created Enode */
        else
        {
                cep = enbase + firstp;
                while( cep->first.ep && cep->operator == E_ARGLIST )
                        cep = enbase + cep->first.ep;

                cep->first.ep = epidx;
        }

        return firstp;
}

Enodep  mk_funccall( Symbol *sp, Enodep arglist)
{
        new_enode();

        ep->operator  = E_CALL;
        ep->first.sp  = sp;
        ep->third.ep  = arglist;
        ep->second.ep = arglist_count(arglist);
        return epidx;
}

/* check actual argument of user functions */
static  int check_farg( Enode *ep    , size_t cnt, Symbol *fsp,
                        Symbol *sumsp, int ldel)
{
        Funcdef *fp  = fsp->u.funp;
        Argdef  **adp= fp->argdp;
        Argdef  *argp= NULL;
        Enode   *argcallp;
        Enodep  argcall;
        int     calltype;
        Symbol  *callsp;
        Variable *vp;
        Param    *pp;
        int      lagtype, lower, upper;
        int vlag, vlead, lower_del;

        argcall  = ep->second.ep;
        argcallp = enbase + argcall;
        calltype = argcallp->operator;

        switch (calltype)
        {
                case E_MVAR : 
                        callsp  = argcallp->first.sp;
                        break;
                case E_PARAM: 
                        callsp  = argcallp->first.sp;
                        break;
                case E_ARG: 
                        callsp = NULL;
                        break;
                default  : 
                        calltype = E_VAL;
                		callsp = NULL;	/* to keep compiler happy; no warning */
                        break;
        }
        argp = adp[cnt];

        /*
         * check argument and call type
         *
         *  Argtype     Meaning
         *  ----------  -----------------------
         *  ARG_VAL     no explicit lag/sum
         *  ARG_REF     explicit lag and/or sum
         *  ARG_REFVAL  only del applied to argument
         *              (del does nothing with a param,
         *              (but applies lag to variable  )
         *
         *  Compare Argtype and Calltype
         *
         *                           Calltype
         *                E_MVAR     E_PARAM     E_ARG        E_VAL
         *                variable   parameter   formal arg.  scalar 
         *  Argtype
         *  ----------
         *  ARG_VAL         ok          ok        ok          ok
         *  ARG_REF         T1          T2        T3          error
         *  ARG_REFVAL      T1          ok        T3          ok
         *
         *  T1 ==> adjust maximum lag/lead of variable
         *  T2 ==> check implied parameter index
         *  T3 ==> update argument types user function and
         *         argument boundaries (lower, lower_del and upper)
         */

        if( argp->atype == ARG_REF && calltype == E_VAL )
        {
            xpcerrmsg("Type mismatch in argument %s in call of %s",
                      fp->argnames[cnt], fsp->name);
            return 1;
        }

        if (calltype != E_VAL) 
        {
            lagtype = argcallp->second.offset;
            lower = upper = argcallp->third.offset;
            if (lagtype == 2) 
            {
                if (sumsp) { 
                    /* lag/lead through sum */
                    lower += sumsp->u.sumvarp->low;
                    upper += sumsp->u.sumvarp->high;
                }
                else 
                   mcerror("Sumsp in arglags == NULL\n");
            }
            lower_del = lower - ldel;
            if (argp->atype == ARG_REF) {
                lower     += argp->lower;
                lower_del += argp->lower_del;
                upper     += argp->upper;
            } 
        }

        /*
         * adjust lags and check param reference
         */


        if (calltype == E_MVAR)
        {
                /*
                 * variable by reference
                 * function applies lags (explicit or implicit)
                 * and maxlag/lead may change
                 */

                vlag = min(lower_del, 0);
                vlead = max(upper, 0);


                vp = callsp->u.varp;

                if (vlead > vp->maxlead) {
                    vp->maxlead = vlead;
                }
                if (vlag < vp->maxlag) {
                    vp->maxlag = vlag;
                }
        }
        else if( calltype == E_PARAM && (pp = callsp->u.parp) != NULL)
        {
                /*
                 * parameter by reference
                 * check implied index
                 */

                if (upper > 0 || abs(lower) > pp->cnt - 1) {
                    par_error(callsp->name, lower, upper, pp->cnt, fsp->name);
                    return 1;
                }
        } 
        else if (calltype == E_ARG) 
        {
            /* we are compiling a user function that is calling another
             * user function. actualp is the actual argument of the 
             * called userfunction and the formal argument of the user 
             * function being compiled. argp is the formal argument of
             * the called user function.
             */
    
            Argdef *actualp = argdefs[argcallp->first.ep];

            if (argp->atype == ARG_REF || lagtype) 
            {
                actualp->atype = ARG_REF;
            }
            else if ((argp->atype == ARG_REFVAL || ldel) 
                       && actualp->atype != ARG_REF) 
            {
                actualp->atype = ARG_REFVAL;
            } else if (actualp->atype == ARG_NOTUSE) {
                actualp->atype = ARG_VAL;
            }

            actualp->lower     = min(lower    , actualp->lower);
            actualp->lower_del = min(lower_del, actualp->lower_del);
            actualp->upper     = max(upper    , actualp->upper);
        }

        return 0;
}

int     check_fcall( Symbol *fsp, Enodep arglist, Symbol *sumsp, int ldel )
{
        /*
         * check function call for
         * number of arguments passed
         * and correct argument types
         *
         * if everything ok returns 1
         * else returns 0
         */

        Funcdef *fp  = fsp->u.funp;
        size_t  cnt  = 0;
        size_t  callcnt;
        Enode   *ep;
        int     rval = 0;

        /*
         * check argument count
         */

        callcnt = arglist_count( arglist );

        if( fp && callcnt != fp->argcnt )
        {
                xpcerrmsg("Function %s requires %u arguments but %u passed",
                        fsp->name, fp->argcnt, callcnt);
                return 0;
        }

        /*
         * check argument types by walking through arglist
         */

        while( arglist )
        {
                ep = enbase + arglist;
                if( ep->operator != E_ARGLIST )
                        mcerror("Missing e_arglist\n");

                rval += check_farg( ep, cnt, fsp , sumsp, ldel);

                arglist = ep->first.ep;         /* next argument */
                ++cnt;
        }

        return rval ? 0 : 1;
}


int  check_lag(Enodep ref, Argdef **argdefs, Symbol *sumsp, int ldel) 
{
   /* check lag of variable occuring in a  expr (expr in an
    * expression except expressions in a user function call).
    * calculate max lead/lag/dellag implied by lag specifier
    * adjust max lead and lag of variables or function argument
    * keep track of argument type (by reference if explicit lag)
    */

    Enode *ep;
    Symbol *sp;
    Argdef *ap;
    int lagtype, lower, upper;

    Variable *vp;
    int lower_del, dlag, lead;

    ep = enbase + ref;

    lagtype = ep->second.offset;
    lower = upper = ep->third.offset;
    lower_del = lower;

    if (lagtype == 2) 
    {
       if (sumsp) { 
           /* lag/lead through sum */
            lower += sumsp->u.sumvarp->low;
            upper += sumsp->u.sumvarp->high;
       }
       else 
           mcerror("Sumsp in arglags == NULL\n");
    }

    lower_del = lower - ldel;

    lead = max(upper, 0);
    dlag = min(lower_del, 0);

    switch( ep->operator)
    {
        case E_ARG : 
                        ap = argdefs[ep->first.ep];
                        ap->lower     = min(lower    , ap->lower);
                        ap->lower_del = min(lower_del, ap->lower_del);
                        ap->upper     = max(upper    , ap->upper);
                            
                        if( lagtype )
                            ap->atype = ARG_REF;
                        else if( ldel && ap->atype != ARG_REF )
                            ap->atype = ARG_REFVAL;
                        else if (ap->atype == ARG_NOTUSE)
                            ap->atype = ARG_VAL;

                        if (options.gen_dep) {
                            mcerror("Dependency information cannot be generated for models with user functions\n");
                        }
                        break;

        case E_PARAM :  sp = ep->first.sp;
                        if (upper > 0 || abs(lower) >= sp->u.parp->cnt) {
                            par_error(sp->name, lower, upper,
                                      sp->u.parp->cnt, NULL);
                            return 1;
                        }
                        break;

        case E_MVAR  :
                        sp = ep->first.sp;
                        vp = sp->u.varp;
                        vp->maxlead = max(lead, vp->maxlead);
                        vp->maxlag  = min(dlag, vp->maxlag );
                        if (options.gen_dep) {
                            add_dependency(sp->name, lower_del, upper);
                        }
                        break;
    }
    return 0;
}



static  int mk_bin_ecode( int op )
{
        int     r;

        switch( op )
        {
                case ','        : r = E_ARGLIST; break;

                case '+'        : r = E_ADD; break;
                case '-'        : r = E_SUB; break;
                case '*'        : r = E_MUL; break;
                case '/'        : r = E_DIV; break;
                case T_POW      : r = E_POW; break;

                case T_AND      : r = E_AND; break;
                case T_OR       : r = E_OR ; break;
                case T_LE       : r = E_LE ; break;
                case T_LT       : r = E_LT ; break;
                case T_GE       : r = E_GE ; break;
                case T_GT       : r = E_GT ; break;
                case '='        : /* fall through!! */
                case T_EQ       : r = E_EQ ; break;
                case T_NE       : r = E_NE ; break;

                default         : r = E_BAD; break;
        }

        return r;
}

static  int mk_un_ecode( int op )
{
        int     r;

        switch( op )
        {
                case '-'        : r = E_NEG; break;
                case T_NOT      : r = E_NOT; break;

                default         : r = E_BAD; break;
        }

        return r;
}

static  int mk_bltin_ecode( int op )
{
        int     r;

        switch( op )
        {
                case XP_BABS    : r = E_ABS  ; break;
                case XP_BATAN   : r = E_ATAN ; break;
                case XP_BCOS    : r = E_COS  ; break;
                case XP_BEXP    : r = E_EXP  ; break;
                case XP_BLOG    : r = E_LOG  ; break;
                case XP_BLOG10  : r = E_LOG10; break;
                case XP_BMIN    : r = E_MIN  ; break;
                case XP_BMAX    : r = E_MAX  ; break;
                case XP_BNINT   : r = E_NINT ; break;
                case XP_BSIN    : r = E_SIN  ; break;
                case XP_BSQRT   : r = E_SQRT ; break;
                case XP_BTAN    : r = E_TAN  ; break;
                case XP_BSINH   : r = E_SINH ; break;
                case XP_BCOSH   : r = E_COSH ; break;
                case XP_BTANH   : r = E_TANH ; break;
                case XP_BASIN   : r = E_ASIN ; break;
                case XP_BACOS   : r = E_ACOS ; break;
                case XP_BTOREAL : r = E_TOREAL;break;
                case XP_BCUMNOR : r = E_CUMNOR ; break;
                case XP_BINVNOR : r = E_INVNOR ; break;
                case XP_BHYPOT  : r = E_HYPOT; break;
                case XP_BFIBUR  : r = E_FIBUR; break;

                default         : r = E_BAD  ; break;
        }

        return r;
}

static  void par_error(char * parname, int lower, int upper, int par_len,
                       char * funcname)
{
    int min;
    char *msg;
    char *funcinfo;
    
    min = -(par_len - 1);

    if (funcname) 
        funcinfo = " due to function call ";
    else 
        funcinfo = "";

    if (lower == upper) {
        msg = "Invalid index (%d) on param %s%s\nThe index should\
 obey %d <= index <= 0";
        xpcerrmsg(msg, lower, parname, funcinfo, min);
    } else {
        msg = "Invalid index range (%d:%d) on param %s%s\nThe index should\
 obey %d <= index <= 0";
        xpcerrmsg(msg, lower, upper, parname, funcinfo, min);
    }

}
