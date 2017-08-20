/*
 *  symbol.h
 *
 *  declarations for symbol table management
 *  prototypes for functions in symbol.c
 *
 */

#ifndef SYMBOL_H
#define SYMBOL_H

#include "dependencies.h"

typedef unsigned int msize_t;
typedef unsigned int uint;
typedef double  real;

typedef struct Param {
        msize_t cnt;
        int   par_index;   /* parameter index */
        union {
                real    dval;   /* if cnt == 1 */
                real    *dp;    /* if cnt  > 1 */
        } u;
} Param;

typedef struct Variable {
        int   vtype;       /* variable type (EXO, ENDO_IDENT, ENDO_FRML) */
        int   var_index;   /* model variable index */
        int   maxlag;
        int   maxlead;
} Variable;

typedef struct SumVariable {
        int   low;
        int   high;
} SumVariable;


typedef struct Argdef {
        int   atype;  /* type of argument (ARG_VAL, ARG_REF etc.) */
        int   lower; /* lower bound offset implied by lag, lead , sum or del,
                      * for variables */       
        int   lower_del; /* lower bound offset lagimplied lag, lead and sum
                       * for parameters (del is not applied to variables) */
        int    upper;  /* upper bound offset implied by lag, lead or sum index  */
} Argdef;

/* argument type definitions */
#define ARG_NOTUSE     -1
#define ARG_VAL         0
#define ARG_REF         1
#define ARG_REFVAL      2
#define NO_ARG          3

/* variable type definitions */
#define EXO         0
#define ENDO_IDENT  1
#define ENDO_FRML   2

typedef struct Funcdef
{
    msize_t argcnt;
    int   func_index;   /* function index  */
    struct {
        unsigned int use_sum    : 1; /* sum used in user function */
        unsigned int use_del    : 1; /* del used in user function */
        unsigned int is_ulfunc  : 1; /* the function is a user language function,
                                        i.e. a function defined in Isis syntax */
    } flags;
    char    **argnames;
    Argdef  **argdp;
    struct  Enode *fcp;     /* pointer to base of code array */
    size_t  estart;         /* offset in fcp of code start   */
}
Funcdef;

typedef struct Equation
{
        int   eqtype;         /* bit 0 set ==> frml     else ident
                                 * bit 1 set ==> implicit else explicit
                                 */
        int     eq_index;       /* equation index */
        struct  Symbol *lhs;
        struct  Enode *ecp;     /* pointer to base of code array */
        size_t  estart;         /* offset in ecp of code start   */
        dependencies *deps;     /* information about dependencies of the model */
}
Equation;

#define Set_Frmleqn(e)  (e)->eqtype |= 1
#define Set_Impleqn(e)  (e)->eqtype |= 2
#define Is_Frmleqn(e)   (e)->eqtype & 1
#define Is_Impleqn(e)   (e)->eqtype & 2

#define HASHSIZE        7919 /*503*/ /* 10007 */  /* 50021 */ /* 90001 */

typedef struct Symbol  /* symbol table entry */
{
    char  *name;
    int   type;       /* lexical type     */
    int   xpctype;

    union
    {
                size_t   handle; /* index into a table */
                Equation    *eqnp;
                Variable    *varp;
                Param       *parp;
                Funcdef     *funp;
                SumVariable *sumvarp;
    }
    u;

    struct Symbol   *next;  /* to link to next */
}
Symbol;

typedef struct symtab_
{
        Symbol  *tab[HASHSIZE];
}
SymTab;

/*
 * return values for function called by symwalk()
 */

#define NXTSYM  0   /* goto next symbol         */
#define DELSYM  1   /* delete current symbol and goto next  */
#define LWQUIT  2   /* quit symwalk             */

/* symbol.c */

SymTab  *sym_init    ( void );
Symbol  *sym_install ( SymTab *, char * , int );
Symbol  *sym_create  ( SymTab *, char * , int );
Symbol  *sym_lookup  ( SymTab *, char * );
Symbol  *sym_tlookup ( SymTab *, char * , int );
void    sym_uninstall( SymTab *, char * );
Symbol  *sym_unlink  ( SymTab *, char * );
void    sym_walk     ( SymTab *, int (*)(Symbol *), Symbol ** );
void    sym_dump     ( SymTab *);
void    sym_stat     ( SymTab *, FILE *);
void    free_symtab  ( SymTab *);

#endif
