/* Yacc syntax
 * for the Isis modelling program
 *
 * uses Bison
 */

%{

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "symbol.h"
#include "mcinit.h"
#include "xpcdef.h"
#include "mchdr.h"
#include "util.h"
#include "ecode.h"
#include "outmdl.h"
#include "macromodtypes.h"
#include "dependencies.h"


void    cleanup( int );
int     check_implicit( int , Symbol *);
int     check_lhs( Symbol * );

void    make_formal( Symbol * );

void    make_param( Symbol * );
int     add_pval( real );
void    set_parval(void);

Enodep  do_var( Symbol *, int, Symbol *, int par);
void    do_lag( Enodep);

void    eqn_prologue(void);
void    eqn_epilogue( Symbol * , int, Symbol *, Enodep , int);

int     sum_prologue( Symbol *, int lo, int hi );
Enodep  sum_epilogue( Enodep );

int     del_prologue( int );
Enodep  del_epilogue( Enodep );

int     ufunc_prologue( Symbol *, int is_ulfunc);
void    ufunc_epilogue( Symbol * , Enodep, int is_ulfunc);
void    funccall_prologue   (void);
Enodep  do_funccall   ( Symbol * , Enodep );
Enodep  do_builtin    ( Symbol * , Enodep );

static  void    eqtyp_err(Symbol *);

#ifdef MCISIS
/* fortran subroutine in mcisis.rf7 */
void FNAME(save_parameter)(FINT *, FREAL8 *);
#endif

%}

%union
{
        int     ival;
        real    dval;
        Symbol  *sp;
        Enodep  ep;
}

%type    <sp>    eqname pname funcname
%type    <sp>    eqtyp_error
%type    <ival>  eqtyp
%type    <ival>  opt_sign '-' '+' sinteger
%type    <dval>  snumber

%type    <sp>    lhsexpl lhsimpl stmt stmtlist model keywd

%type    <ep>    var fargs ufargs ufarg rexpr lexpr sexpr expr ifexpr ifthen 
%type    <ep>    ifpart1 elseif elseifl

%token   <sp>    T_END "end"
%token   <sp>    T_FUNCTION "function" T_UL_FUNCTION "ul function"
%token   <sp>    T_FRML "frml" T_IDENT "ident" T_PARAM "param"
%token   <sp>    T_NAME "name" T_UFUNC "user function"
%token   <sp>    T_ULFUNC "user language function"

%token   <sp>    T_IF "if" T_THEN "then" T_ELSE "else" T_ELSEIF "elseif" 
%token   <sp>    T_ENDIF "endif"
%token   <sp>    T_SUM "sum" T_DEL "del"
%token   <sp>    T_BUILTIN "builtin function"

%token   <dval>  T_NUMBER "number"
%token   <ival>  T_INTNUM "integer"

%token   <ival>  T_GE ">=" T_GT ">" T_LT "<"
%token   <ival>  T_LE "<=" T_EQ ".eq." NE "^=" 
%token   T_NOT "^ or .not." T_AND "& or .and." T_OR "| or .or."

/*
 * declare associativity and precedence for yacc
 * relational operators such as < .lt. are not
 * nonassociative (should be?)
 */

%left   T_IF T_THEN T_ELSE T_ELSEIF
%left   T_OR
%left   T_AND
%left   T_NOT '^'
%left   T_GT T_GE T_LT T_LE T_EQ T_NE '='
%left   '+' '-'
%left   '*' '/'
%left   UNARYMINUS
%right  T_POW

/*
 * one harmless shift / reduce conflict is expected, 
 * due to the optional endif keyword */
%expect 1

%error-verbose

%%

model   : stmtlist opt_end
        ;

opt_end : /* nix */
        | T_END ';' {reset_scanner(); YYACCEPT;} /* ignore all following stuff */
        ;

stmtlist: stmt
        | stmtlist stmt
        ;

stmt    : T_FUNCTION fstmt    
        | T_UL_FUNCTION ulfstmt
        | T_PARAM    plist  ';'

        | eqtyp 
          lhsexpl '=' expr  ';'   { eqn_epilogue(NULL, $1, $2, $4, 0);}

        | eqtyp 
          lhsimpl '=' expr  ';'   { eqn_epilogue(NULL, $1, $2, $4, 1);}


        | eqtyp eqname
          lhsexpl '=' expr  ';'   { eqn_epilogue($2, $1, $3, $5, 0);}

        | eqtyp eqname
          lhsimpl '=' expr  ';'   { eqn_epilogue($2, $1, $3, $5, 1);}

        | /* nix */         ';'   { $$ = NULL;}
        | error             ';'   { cleanup(0); yyerrok; }
        ;

eqtyp   : T_FRML        { eqn_prologue(); $$ = 1;}
        | T_IDENT       { eqn_prologue(); $$ = 0;}
        | eqtyp_error   { /* error: illegal equation type */
                          eqtyp_err($1); $$ = 0;}
        ;

eqtyp_error : T_NAME   {$$ = $1;}
            | keywd    {$$ = $1;}
            | T_UFUNC  {$$ = $1;}
            | T_ULFUNC {$$ = $1;}
            ;

eqname  : T_NAME  
        | keywd     /* the equation name may be a key word (e.g. "IF")
                     * equation are stored in a separate symbol table */
        | T_UFUNC   /* the equation name may also be equal to the name
                     * of  user function */
        | T_ULFUNC   /* the equation name may also be equal to the name
                     * of  user language function */
        ;

keywd   : T_BUILTIN   {$$=$1;}
        | T_SUM       {$$=$1;}
        | T_DEL       {$$=$1;}
        | T_IF        {$$=$1;}
        | T_ELSE      {$$=$1;}
        | T_ELSEIF    {$$=$1;}
        | T_THEN      {$$=$1;}
	;

plist   : pnlist
        | plist pnlist
        ;

pnlist  : pname  nlist  { set_parval(); }
        ;

pname   : T_NAME        { make_param($1);}
        ;

nlist   : snumber        { if (add_pval($1)) YYERROR;}
        | nlist snumber  { if (add_pval($2)) YYERROR;}
        ;

snumber : opt_sign T_NUMBER { $$ = ($1 == '-') ? -$2 : $2; }
        | sinteger           {$$ = $1;}
        ;

sinteger : opt_sign T_INTNUM { $$ = ($1 == '-') ? -$2 : $2; }
        ;

/*
 * syntax for second alternative is 0( Name )
 */

lhsexpl : T_NAME                  { check_lhs($1); }
        | T_UFUNC  /* ERROR; call check_lhs for nice error message */  
                                  { check_lhs($1); }  
        | T_ULFUNC  /* ERROR; call check_lhs for nice error message */  
                                  { check_lhs($1); }  
        | keywd   /* ERROR: call check_lhs for nice error message */
                                  { check_lhs($1); }  
        ;

lhsimpl : T_INTNUM '(' T_NAME ')'  { $$ = $3; if(check_implicit($1, $3)) YYERROR; }
        | T_INTNUM '(' T_UFUNC ')'
                              /* ERROR; call check_lhs for nice error message */  
                                { $$ = $3; if(check_implicit($1, $3)) YYERROR; }
        | T_INTNUM '(' T_ULFUNC ')'
                              /* ERROR; call check_lhs for nice error message */  
                                { $$ = $3; if(check_implicit($1, $3)) YYERROR; }
        ;

/*
 * the expr may not contain a call to a user defined function
 * and may not imply nesting of Del and/or Sum functions
 */

funcname : T_NAME                   
         | T_UFUNC   /* function name already defined */
         | T_ULFUNC  /* function name already defined */
         ;

fstmt   : funcname                 { if (ufunc_prologue($1, 0)) YYERROR;}
          '(' formals ')' '=' expr { ufunc_epilogue($1, $7, 0); }
        ;

ulfstmt   : funcname      { if (ufunc_prologue($1, 1)) YYERROR;}
          '(' formals ')' { ufunc_epilogue($1, 0, 1); }
        ;

formals : T_NAME                   { make_formal($1); }
        | formals ',' T_NAME       { make_formal($3); }
        ;

/*
 * the expr in del   may not contain another T_DEL
 * the expr in sum   may not contain another T_SUM
 */

/* scalar expression */
sexpr   : T_NUMBER                         { $$ = mk_const($1); }
        | T_INTNUM                         { $$ = mk_const($1); }

        | T_BUILTIN '(' fargs ')'          { $$ = do_builtin($1, $3); }
        | T_UFUNC   '(' ufargs ')'         { 
                              if (($$ = do_funccall($1, $3)) == 0) YYERROR;
                                           }
        | T_ULFUNC   '(' ufargs ')'         { 
                              if (($$ = do_funccall($1, $3)) == 0) YYERROR;
                                           }

        | T_DEL '(' delarg ':' expr ')'    { $$ = del_epilogue($5); }
        | T_SUM '(' sumarg ':' expr ')'    { $$ = sum_epilogue($5); }

        | ifexpr            /* T_ENDIF is optional for compatibilty */
        | ifexpr T_ENDIF
        | rexpr
        ;

expr    : sexpr
	| var                              { do_lag($1);}
        ;

ifexpr  : ifpart1 T_ELSE expr   {$$ = add_elsepart($1, $3);}
        ; 

ifpart1 : ifthen                    {$$ = $1;}
        | ifthen elseifl            {$$ = add_elsepart($1, $2);}

ifthen  : T_IF  expr T_THEN expr    {$$ = mk_ifnode($2, $4);}
        ;

elseif  : T_ELSEIF expr T_THEN expr {$$ = mk_ifnode($2, $4);}
        ;

elseifl : elseif          {$$ = $1;}
        | elseifl elseif  {$$ = add_elsepart($1, $2);}
        ;

delarg  : T_INTNUM { if( del_prologue($1) ) YYERROR; }
        ;

sumarg  : T_NAME '=' sinteger ',' sinteger
                                       { if( sum_prologue($1, $3, $5) ) YYERROR; }
        ;

rexpr   : '(' expr ')'                  { $$ = $2; }
        | expr  '+'   expr              { Binary :
                                                  $$ = mk_binenode($2, $1, $3);
                                        }
        | expr  '-'   expr              { goto Binary; }
        | expr  '*'   expr              { goto Binary; }
        | expr  '/'   expr              { goto Binary; }
        | expr T_POW  expr              { goto Binary; }
        | '-' expr  %prec UNARYMINUS    { Unary :
                                                 $$ = mk_unop($1, $2);
                                        }
        | '+' expr  %prec UNARYMINUS    { $$ = $2; }
        | lexpr
        ;

lexpr   : expr T_GT  expr               { goto Binary; }
        | expr T_GE  expr               { goto Binary; }
        | expr T_LT  expr               { goto Binary; }
        | expr T_LE  expr               { goto Binary; }
        | expr '='   expr               { goto Binary; }
        | expr T_EQ  expr               { goto Binary; }
        | expr T_NE  expr               { goto Binary; }
        | expr T_AND expr               { goto Binary; }
        | expr T_OR  expr               { goto Binary; }
        | T_NOT expr                    { goto Unary;  }
        | '^' expr %prec T_NOT          { $$ = mk_unop(T_NOT, $2); }
        ;

/*
 * non empty comma separated actual args to builtin function
 */

fargs   : expr           { $$ = mk_fargs(0 , $1 ); }
        | fargs ',' expr { $$ = mk_fargs($1, $3 ); }
        ;

/*
 * non empty comma separated actual args to user function
 */

ufargs   : ufarg             { $$ = mk_fargs(0 , $1 ); }
         | ufargs ',' ufarg  { $$ = mk_fargs($1, $3 ); }
         ;
/*
 * user function argument. we do not use ufarg : expr, because
 * procedure do_lag should not be called for formal arguments
 */
ufarg   : sexpr
        | var
        ;

/*
 * refers to variable, parameter or function args
 * lags may only be < 0 for parameters
 * the last three may only occur in a sum expression
 */

var     : T_NAME                               { $$ = do_var($1,  0, 0 , 0); }
        | T_NAME '['  sinteger             ']' { $$ = do_var($1, $3, 0 , 0); }
        | T_NAME '['  T_NAME               ']' { $$ = do_var($1,  0, $3, 0); }
        | T_NAME '['  T_NAME '-' T_INTNUM  ']' { $$ = do_var($1,-$5, $3, 0); }
        | T_NAME '['  T_NAME '+' T_INTNUM  ']' { $$ = do_var($1, $5, $3, 0); }

        /* Obsolete notation using round brackets () */
        | T_NAME '('  sinteger             ')' { $$ = do_var($1, $3, 0 , 1); }
        | T_NAME '('  T_NAME               ')' { $$ = do_var($1,  0, $3, 1); }
        | T_NAME '('  T_NAME '-' T_INTNUM  ')' { $$ = do_var($1,-$5, $3, 1); }
        | T_NAME '('  T_NAME '+' T_INTNUM  ')' { $$ = do_var($1, $5, $3, 1); }
        ;

opt_sign: /* nix */ { $$ = 0; }
        | '-'
        | '+'
        ;

        ;


%%

/*
 * flags and variables for checking
 *
 */

static  int     insum = 0;      /* 1 when doing Sum(...) */
static  int     indel = 0;      /* 1 when doing Del(...) */
static  int     indef = 0;      /* 1 when doing Function */

static  int     deloffset = 0;

static  int     Curfunc_sum = 0;
static  int     Curfunc_del = 0;

static  char    *Curfunc = NULL;
static  int     in_equation = 0;
static  Symbol  *sumvar  = NULL;

static  char    **argnames = NULL;
Argdef  **argdefs  = NULL;
static  size_t  argcnt = 0;

static int      varCount = 0;  /* count of model variables */
static int      eqCount = 0;   /* count of equations */
static int      parCount = 0;  /* count of model parameters */
static int      funcCount = 0; /* count of user functions  */
static int      ulFuncCount = 0; /* count of user language function */
/* Parameters */

static  size_t  curpcnt = 0;       /* count of numbers in pval             */
static  double  pval[MAXPARAM]; /* temp storage for param values        */
static  Symbol *Curparam;


/*
 * prototypes
 */

static  void new_var( Symbol *, int , int);

/*
 * the following is for "semantic" error checks
 *
 * all error messages and error flags setting
 * are handled through xpcerrmsg(...)
 *
 * all routines and actions taken have been setup
 * in such a way that they are robust wrt errors
 *
 * the Enode generating functions are error resistant
 */

static  int     errflag = 0;
static  void    name_err ( char *, Symbol *);

void    cleanup( int comefrom )
{
    /*
     * comefrom == 0 called by yacc error production
     * else from one of the following functions
     */

    size_t  i;

    if( sumvar )
    {
        efree( sumvar->u.sumvarp );
        sym_uninstall( Stp, sumvar->name );
        sumvar = NULL;
    }

    if( argnames )
    {
        for( i = 0; i < argcnt ; i++ ) {
            sym_uninstall( Stp, argnames[i]);
            /* note: argnames[i] points to the name field of the
             * symbol with name argnames[i]: therefore argnames[i]
             * should not be freed (the memory is already freed
             * in sym_uninstall) */
        }
        argcnt = 0;
        free(argnames);
        argnames = NULL;
    }

    if( errflag )
    {
        if( Curfunc || in_equation )
            reset_enode();
    }

    if( comefrom == 0 )
    {
        if( Curfunc )
            sym_uninstall(Stp, Curfunc);
    }

    if (argdefs) {
       free(argdefs);
    }

    errflag = 0;
    Curfunc = NULL;
    in_equation = 0;
    argdefs = NULL;
    insum   = indel = indef = Curfunc_sum = Curfunc_del = 0;
}

int  ufunc_prologue(Symbol *sp, int is_ulfunc) {

    if (sp->xpctype != XP_UNDEF) {
	/* function name already used for something else */
        name_err(sp->name, sp);
	return 1;
    }
	
    indef    = 1;
    Curfunc  = sp->name;

    if( strlen(Curfunc) > MAX_UFNNLEN )
        warning("Function name <%s> too long (max %d chars)\n",
                Curfunc, MAX_UFNNLEN );
	
    if (is_ulfunc) {
        sp->type    = T_ULFUNC;
        sp->xpctype = XP_ULFUNC;
    } else {
        sp->type    = T_UFUNC;
        sp->xpctype = XP_FUNC;
    }
    sp->u.funp  = NULL;

    if (!is_ulfunc) {
        open_enode();
    }

    return 0;
}

void    ufunc_epilogue( Symbol *sp, Enodep ex, int is_ulfunc)
{
    size_t  i;
    Funcdef *fnp;

    if( errflag )
    {
        sym_uninstall(Stp, Curfunc);        /* delete function */
        cleanup(1);
        sp->u.funp = NULL;
        return;
    }

    sp->u.funp   = fnp = emalloc( sizeof(Funcdef) );
    fnp->flags.is_ulfunc = is_ulfunc;
    fnp->flags.use_sum = Curfunc_sum;
    fnp->flags.use_del = Curfunc_del;

    fnp->argcnt   = argcnt;
    fnp->argnames = emalloc( argcnt * sizeof(char*) );
    fnp->argdp    = argdefs;

    if (is_ulfunc) {
	    fnp->func_index = ulFuncCount++; 
            /* user language function index (first function has index 0) */
    } else {
        fnp->func_index = funcCount++; /* function index (first function has 
                                           index 0) */
    }

    if( argnames )
    {
        for( i = 0; i < argcnt ; i++ ) {

            /* copy argnames to func def
             * and only then uninstall from symbol table
             */
            fnp->argnames[i] = estrdup( argnames[i] );
            sym_uninstall( Stp, argnames[i]);

            /* note: argnames[i] points to the name field of the
             * symbol with name argnames[i]: therefore argnames[i]
             * should not be freed (the memory is already freed
             * in sym_uninstall) */
        }

        free(argnames);
    }

    if (!is_ulfunc) {
        fnp->fcp    = close_enode();    /* save code of func expression */
        fnp->estart = ex;
    } else {
        fnp->fcp    = NULL;
        fnp->estart = -1;
    }

    indef   = 0;
    Curfunc_sum = Curfunc_del = 0;
    Curfunc = NULL;
    argcnt = 0;
    argnames = NULL;
    argdefs = NULL;

#ifdef MCISIS
     if (!is_ulfunc) {
         if (errcnt == 0 && warncnt == 0) {
	     out_ipcode(sp);
         }
         fnp->fcp = NULL;
     }
#else
     new_eqn(sp);
#endif
}

Enodep  do_builtin( Symbol *sp, Enodep arglist )
{
    size_t  largcnt = arglist_count(arglist);

    if( XP_builtin2p(sp->xpctype) )
    {
        if( largcnt < 2 )
            xpcerrmsg("Builtin <%s> requires at least 2 arguments",
                        sp->name);
    }
    else if( XP_builtin1(sp->xpctype) )
    {
        if( largcnt != 1 )
            xpcerrmsg("Builtin <%s> requires 1 argument",
                        sp->name);
    }
    else if( XP_builtin2(sp->xpctype) )
    {
        if( largcnt != 2 )
            xpcerrmsg("Builtin <%s> requires 2 arguments",
                        sp->name);
    }
    else
        xpcerrmsg("Unknown builtin <%s>", sp->name);

    return mk_builtin(sp, arglist);
}

Enodep  do_funccall( Symbol *sp, Enodep arglist)
{

    /*
     * Handle function call. Returns an pointer to the enode tree
     * of the function call, or 0 if an error has occurred and parsing
     * of the current statement should be aborted.
     */

    if (indel && sp->u.funp->flags.use_del) {
        xpcerrmsg( "Use of function %s implies a nested DEL",
                    sp->name);
    }

    if (insum && sp->u.funp->flags.use_sum) {
        xpcerrmsg( "Use of function %s implies a nested SUM",
                    sp->name);
    }

    if (Curfunc != NULL && strcmp(sp->name, Curfunc) == 0) {
        xpcerrmsg("Function %s calls itself. Recursion is not allowed",
                  sp->name);
	/* stop parsing current equation or function */
        return 0;
    }

    /* check number of arguments passed and argument types.
     * do not stop parsing the current equation or function in case of an error */
    check_fcall(sp, arglist, sumvar, deloffset);

    return mk_funccall(sp, arglist);
}

void eqn_prologue (void)
{
    open_enode();
    in_equation = 1;
}

void  eqn_epilogue(Symbol *eqnamesp, int isfrml, Symbol *lhs, Enodep ex,
                       int isimplicit) {
    Equation *eqnp;
    Symbol *eqsp;
    char *eqn_name, *p;

    if (eqnamesp != NULL) {
        eqn_name = eqnamesp->name;
        if ((eqnamesp->type == T_BUILTIN) || (eqnamesp->xpctype == XP_CMD)) {
           /* the name of the equation is a keyword: convert to uppercase */
           for (p = eqn_name; (*p = tolower(*p)) != '\0'; p++);
        }
    } else 
        eqn_name = lhs->name;

    eqn_name = (eqnamesp != NULL) ?  eqnamesp->name : lhs->name;
	
    eqsp = sym_create(Eqntp, eqn_name, T_NAME); 

    /* remove unused name from symol table. */
    if (eqnamesp != NULL && eqnamesp->xpctype == XP_UNDEF) {
        sym_uninstall(Stp, eqn_name);
    }

    if( eqsp->xpctype != XP_UNDEF )
        xpcerrmsg( "Duplicate equation %s", eqsp->name);


    eqsp->xpctype = XP_EQN;

    if (errflag) {
        cleanup(1);
	eqsp->u.eqnp = NULL;
        return;
    }

    eqsp->u.eqnp = eqnp = emalloc( sizeof(Equation) );

    eqnp->eqtype = 0;
    eqnp->eq_index = eqCount++; /* equation index (first equation has index 0) */
    if( isfrml ) {
        Set_Frmleqn(eqnp);
    } 
    if( isimplicit ) {
        Set_Impleqn(eqnp);
    }

    lhs->u.varp->vtype = isfrml ? ENDO_FRML : ENDO_IDENT;

    eqnp->lhs    = lhs;
    eqnp->ecp    = close_enode();
    eqnp->estart = ex;

    in_equation = 0;

#ifdef MCISIS
    if (errcnt == 0 && warncnt == 0) {
        out_ipcode(eqsp);
    }
    eqnp->ecp    = NULL;
#else
    new_eqn(eqsp);
#endif

    if (options.gen_dep) {
        eqnp->deps = close_dependencies();
    } else {
        eqnp->deps = NULL;
    }
}

int     sum_prologue( Symbol *sp, int lo, int hi )
{
    /*
     * create sum variable
     */

    SumVariable *vp;

    if( insum )
    {
        xpcerrmsg( "Nested SUM not allowed\n" );
        return 1;
    }

    if( indef )
        Curfunc_sum = 1;

    insum = 1;

    if( sp->xpctype != XP_UNDEF )
    {
        /*
         * already exists as something else
         * create new purely as local variable
         */

        sumvar = sym_install( Stp, sp->name, T_NAME);
    }
    else
        sumvar = sp;

    /*
     * set type to sum variable
     */

    sumvar->xpctype = XP_SVAR;

    sumvar->u.sumvarp  = vp = emalloc( sizeof(SumVariable) );

    if( lo > hi )
        xpcerrmsg( "Invalid sum index range\n" );

    vp->low  = lo;
    vp->high = hi;

    return 0;
}

Enodep  sum_epilogue( Enodep sumexpr )
{
    /*
     * Symbol structure for a sum index may not
     * be deleted but must be unlinked
     * this is to remember the name of the sum index
     */

    Enodep rval;
    Symbol  *sp;

    if (sumvar) {
        sp = sym_unlink( Stp, sumvar->name );
        sumvar = NULL;
        rval = mk_sum( sp, sumexpr);
        /*
         * NOTE: memory leak in this code. sumvar is not any more in the
         *       symbol table, so the associated memory is not released
         *       in function free_symtab (see init.c). However, when the 
         *       compiler is used to compile Isis code, the memory is 
         *       released in outipcode.c. 
         */
    } else {
        rval = 0;
    }

    insum = 0;
    return rval;
}

int     del_prologue( int delarg )
{
    if( indel )
    {
        xpcerrmsg( "Nested DEL not allowed\n" );
        return 1;
    }

    if( indef )
        Curfunc_del = 1;

    indel = 1;

    if( delarg == 0 )
        xpcerrmsg( "Zero del not possible\n" );

    deloffset = delarg;

    return 0;
}

Enodep  del_epilogue( Enodep delexpr)
{
    Enodep  rval = mk_del(deloffset, delexpr);

    indel = deloffset = 0;
    return rval;
}

int check_implicit( int d , Symbol *sp )
{
    if( d != 0 ) {
        xpcerrmsg( "Implicit equation must be 0(name)\n" );
        return 1;
    }

    return check_lhs(sp);
}

int  check_lhs( Symbol *sp )
{
    /*
     * check if lhs variable is already endogenous
     * if it is then it has already been used on lhs
     * for the time being use xpctype field
     */

    if (sp->type == T_BUILTIN || sp->xpctype == XP_CMD) {
        xpcerrmsg( "error: %s is a reserved word", sp->name);
        return 1;
    } else if( sp->xpctype == XP_ENDO ) {
        xpcerrmsg( "Variable %s used more than once in left hand side",
                            sp->name);
        return 1;
    } else if( sp->xpctype != XP_UNDEF && sp->xpctype != XP_EXO ) {
        /* the name is aleady used for something else */
        name_err(sp->name, sp);
        return 1;
    } else if( sp->xpctype == XP_UNDEF )
        /* for the time being assume that variable has type ENDO_IDENT,
         * the correct type will be set in eqn_epilogue */
        new_var( sp, XP_ENDO, ENDO_IDENT);
    else
    {
        sp->xpctype       = XP_ENDO;
    }
    return 0;
}

void    make_formal( Symbol *sp )
{
    /*
     * create a new formal argument for a function
     */

    Argdef  *argp;

    if( sp->xpctype == XP_FNARG )
        xpcerrmsg( "duplicate argument %s in function %s\n",
                            sp->name, Curfunc);
    else if( sp->xpctype == XP_UNDEF )      /* totally new name */
        sp->xpctype = XP_FNARG;
    else
    {   /* make a new copy */

        sp = sym_install( Stp, sp->name, T_NAME );
        sp->xpctype = XP_FNARG;
    }

    sp->u.handle = argcnt; /* offset base 0 */

    argnames = erealloc( argnames, (argcnt + 1) * sizeof( char   * ) );
    argdefs  = erealloc( argdefs , (argcnt + 1) * sizeof( Argdef * ) );

    argp = argdefs[argcnt] = emalloc( sizeof(Argdef) );
    argp->atype   = ARG_NOTUSE;
    argp->lower      = INT_MAX;
#ifndef MCISIS
    argp->lower_del  = INT_MAX;
#endif
    argp->upper      = INT_MIN;

    argnames[argcnt++] = sp->name;
}

void do_lag(Enodep ep)
/*
 * check lags applied to variables that occur in an expr.
 * calculate max lead/lag/dellag implied by lag specifier
 * adjust max lead and lag of variables or function argument
 * keep track of argument type (by reference if explicit lag)
 */
{
    check_lag(ep, argdefs, sumvar, deloffset);
}

static  void new_var( Symbol *sp , int xpctype, int vtype )
{
    Variable *vp;

    if( strlen(sp->name) > MAX_VARNLEN )
        warning("Variable name <%s> too long (max %d chars)\n",
                    sp->name , MAX_VARNLEN );

    sp->u.varp  = vp = emalloc( sizeof(Variable) );
    sp->xpctype = xpctype;

    vp->vtype   = vtype;
    vp->var_index = varCount++; /* variable index (first variable has index 0) */
#ifndef MCISIS
    vp->maxlead = 0;
    vp->maxlag  = 0;
#endif
}

static  void check_var( Symbol *sp )
{

    /*
     * create a variable
     * may be sum variable or local
     *
     */

    if( sp->xpctype != XP_UNDEF && sp->xpctype != XP_SVAR &&
        sp->xpctype != XP_FNARG && sp->xpctype != XP_EXO  &&
        sp->xpctype != XP_ENDO  && sp->xpctype != XP_PARAM
      ) {
        name_err(sp->name, sp);
    } else if( sp->xpctype == XP_UNDEF )
        /* for the time being assume that variable is exogeneous */
        new_var(sp, XP_EXO, EXO);

}

Enodep  do_var( Symbol *sp, int lagoffset , Symbol *sumsp, int parentheses)
{
    int     lagtype = 0;

    check_var(sp);

    if( sumsp )
    {
        if( !insum ) {
            if (parentheses) {
                xpcerrmsg( "Unknown function %s", sp->name);
            } else {
               xpcerrmsg( "Invalid variable lag %s. Variable lags may only be used in sum expression", sumsp->name);
            }
            return 0;
        } else if( sumsp->xpctype != XP_SVAR ) {
            if (parentheses) {
                xpcerrmsg( "Unknown function %s", sp->name);
            } else {
                xpcerrmsg( "Invalid variable lag %s. Only a sum index may be used as variable lag", sumsp->name);
            }
            return 0;
        }
    }

    if( (lagoffset || sumsp != NULL) && sp->xpctype == XP_SVAR )
        xpcerrmsg("Sum index cannot be lagged\n");

    if (sumsp)
        lagtype = 2;
    else if (lagoffset != 0)
        lagtype = 1;
    else
        lagtype = 0;

    return mk_var( sp, lagtype, lagoffset);
}

static  void name_err( char *name, Symbol *sp)
{
    char * typename;

    switch (sp->xpctype) {
    case XP_PARAM:
       typename = "parameter";
       break;
    case XP_FUNC:
       typename = "user function";
       break;
    case XP_ENDO:
       typename = "endogenous variable";
       break;
    case XP_EXO:
       typename = "exogenous variable";
       break;
    default:
      typename = NULL;
      break;
    }
    
    if (typename != NULL)
        xpcerrmsg("Name %s already used as %s name", name, typename);
    else   
        xpcerrmsg("Name %s already used for something else", name);
}

static  void eqtyp_err(Symbol *sp) {
    xpcerrmsg("Illegal equation type %s", sp->name);
}

void    xpcerrmsg( char *fmt, ... )
{
    va_list ap;

    va_start(ap, fmt);
        mcerrmessage(fmt, ap);
    va_end(ap);
    ++errflag;
}

void make_param(Symbol *sp)
{
    /*
     * create a parameter variable
     */

    curpcnt = 0;
    Curparam = sp;

    /*
     * for non-strict compilation (parameters may be used
     * before they have been defined), the model has already
     * been scanned for parameters. All parameters are already 
     * in the symbol table
     */
    if (!options.Strict) {
       return;
    }

    /* the following code is for the strict compilation */

    char *pname = sp->name;

    if (sp->xpctype == XP_PARAM )
    {
        mcerror( "Duplicate parameter name %s", sp->name);
    }
    else if (sp->xpctype != XP_UNDEF)
    {
        /* parameter name used for something else */
        name_err(sp->name, sp);
    }

    if( strlen(pname) > MAX_PARNLEN ) {
        mcerror("Parameter name %s too long (max %d chars)",
                    pname, MAX_PARNLEN);
    }

    sp->xpctype = XP_PARAM;
    sp->u.parp = NULL;
}

int add_pval(double rval) {
    /* adds a value to the current parameter. returns 0 if succesfull,
     * or 1 if the number of values > MAXPARAM */
    if( curpcnt >= MAXPARAM ) {
        mcerror( "Too many parameter values for parameter %s. The maximum is %d.",
                  Curparam->name, MAXPARAM);
        return 1;
    }

    pval[curpcnt++] = rval;
    return 0;
}

void set_parval()
{
    /*
     * create new parameter
     * save parameter values
     */

     Param *p;

     if (options.Strict) {
         /* create a Param, and set its length.
          * (for non-strict compilation this has already
          * been done while scanning parameters */
         Curparam->u.parp = p = emalloc( sizeof(Param) );
         p->cnt = curpcnt;
         p->par_index = parCount++; /* variable index (first variable has index 0) */
     }

#ifdef MCISIS
     FNAME(save_parameter)((FINT *) &curpcnt, pval);
#else
     p = Curparam->u.parp;
     if (curpcnt == 1 ) {
         p->u.dval = pval[0];
     } else {
         p->u.dp = emalloc( sizeof(real) * curpcnt );
         memcpy( p->u.dp, pval, sizeof(real) * curpcnt );
     }
     new_par(Curparam);
#endif

#if 0
        fprintf( stderr, "Parameter name %s # of values %u\n",
                        Curparam->name , curpcnt);
        if( curpcnt == 1 )
                fprintf(stderr, "%10.4f\n", p->u.dval );
        else
        {
                size_t i;
                for( i = 0; i < curpcnt ; i++ )
                {
                        fprintf(stderr, "%10.4f", p->u.dp[i] );
                        if( (i + 1) % 8 == 0 )
                                fprintf( stderr, "\n" );
                }
                fprintf(stderr, "\n" );
        }
#endif
}

void mcparse_init(void) 
    /* initialise variables for parser */
{
   eqCount = 0;
   varCount = 0;
   parCount = 0;
   funcCount = 0;
   ulFuncCount = 0;
}
