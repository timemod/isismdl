/*
 * typedefs of expression nodes
 */
#ifndef IPCODE_H
#include "ipcode.h"
#endif

#ifndef ENODE_H
#define ENODE_H

typedef union   Item
{
        Symbol  *sp;    /* Symbol pointer to a symbol table
                         * for variables/params/functions
                         */

        int     offset; /* lags/leads or lag/lead type  */
        real    rval;   /* constants                    */
        size_t  ep;     /* enode index                  */
}
Item;

typedef struct  Enode
{
        int     operator;
        Item    first, second, third;
}
Enode;

typedef size_t  Enodep;

/*
 * prototypes
 */

void    open_enode ( void );
Enode  *close_enode( void );
void    reset_enode( void );
void    free_enodes( void );

Enodep  mk_binenode( int , Enodep, Enodep );
Enodep  mk_const   ( real );
Enodep  mk_unop    ( int , Enodep );
Enodep  mk_ifnode  ( Enodep, Enodep);
Enodep  add_elsepart( Enodep, Enodep);
Enodep  mk_del     ( int, Enodep );
Enodep  mk_sum     ( Symbol *, Enodep );
Enodep  mk_var     ( Symbol *, int, int);
Enodep  mk_builtin ( Symbol *, Enodep );
Enodep  mk_funccall( Symbol *, Enodep );
Enodep  mk_fargs   ( Enodep  , Enodep );

size_t  arglist_count( Enodep );
int     check_fcall( Symbol *, Enodep, Symbol *, int);
int     check_lag(Enodep, Argdef **, Symbol *, int);

/*
 * define E_... codes
 * E codes < 1000 have same integer constant for corresponding IP-code
 * in ipcode.h .
 * E-codes >= 1000 do not have a corresponding code in ipcode.h
 */

#define E_RCONST        IP_LIRL
#define E_MVAR          IP_LIV0
#define E_SVAR          IP_LISUM
#define E_PARAM         IP_LIP0
#define E_SUM           IP_SUM
#define E_DEL           IP_DEL
#define E_IF            IP_IF
#define E_ARG           1000
#define E_CALL          IP_CALL
#define E_ARGLIST       1001

#define E_ADD          IP_ADD       /* + */
#define E_SUB          IP_SUB 
#define E_MUL          IP_MUL 
#define E_DIV          IP_DIV
#define E_POW          IP_POW
#define E_NEG          IP_NEG
#define E_LT           IP_LT
#define E_LE           IP_LE
#define E_GT           IP_GT
#define E_GE           IP_GE
#define E_EQ           IP_EQ
#define E_NE           IP_NE
#define E_AND          IP_AND
#define E_OR           IP_OR
#define E_NOT          IP_NOT

#define E_ABS          IP_ABS
#define E_ATAN         IP_ATAN
#define E_COS          IP_COS
#define E_EXP          IP_EXP
#define E_LOG          IP_LOG
#define E_LOG10        IP_LOG10
#define E_MIN          IP_MIN
#define E_MAX          IP_MAX
#define E_NINT         IP_NINT
#define E_SIN          IP_SIN
#define E_SQRT         IP_SQRT
#define E_TAN          IP_TAN
#define E_SINH         IP_SINH
#define E_COSH         IP_COSH
#define E_TANH         IP_TANH
#define E_ASIN         IP_ASIN
#define E_ACOS         IP_ACOS
#define E_TOREAL       IP_TORL
#define E_CUMNOR       IP_CUMNOR
#define E_INVNOR       IP_INVNOR
#define E_HYPOT        IP_HYPOT
#define E_FIBUR        IP_FIBUR

#define E_GOTO         1002 // ??? where used

#define E_STOP        IP_STOP
#define E_START       1003
#define E_BAD         1004

// E_VAL is used in check_farg
#define E_VAL         2001

#endif
