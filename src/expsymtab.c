/*
 * Export symbol table from model compiler to DA dynamic arrays 
 * in fortran code. Function export_symtab is used when the xpc
 * compiler is used to compile a model in Isis.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "macromodtypes.h"
#include "symbol.h"
#include "xpcdef.h"
#include "mcinit.h"

/* 
 * Declaration of Fortran subroutines (see file src/libf7/mcisis.rf7)
 * called in the functions in this file.
 */
void FNAME(mc_alloc)(MCINT *, MCINT *, MCINT *, MCINT *, MCINT *, 
                     MCINT *, MCINT *, MCINT *, MCINT *, MCINT *,
                     MCINT *, MCINT *, MCINT *, MCINT *);
void FNAME(add_eqname)(MCINT *, MCINT *, FUCHAR *, MCINT *, FUCHAR *);
void FNAME(add_varname)(MCINT *, MCINT *, FUCHAR *, FUCHAR *);
void FNAME(add_parname)(MCINT *, MCINT *, FUCHAR *);
void FNAME(add_funcname)(MCINT *, MCINT *, FUCHAR *, MCINT *);
void FNAME(add_ulfuncname)(MCINT *, MCINT *, FUCHAR *);

static int eqCount, varCount, parCount, coefCount, funCount, ulFunCount,
           eqCharCount, varCharCount, parCharCount, funCharCount,
           ulFunCharCount, caCount, leadVarCount;

/* count equation names */
static int countEquations(Symbol *sp) {
    eqCount++;
    eqCharCount += strlen(sp->name);
    return NXTSYM;
}

/* pass equation name to isis compiler code */
static MCINT addEquationName(Symbol *sp) {

    MCINT eq_index, lhs_num, name_len;
    char type;

    Equation *eqnp;
    Symbol *lhs;
    Variable *lhs_var;

    eqnp = sp->u.eqnp;
    eq_index = (MCINT) eqnp->eq_index + 1;
    name_len = (FINT) strlen(sp->name);
    lhs = eqnp->lhs;
    lhs_var = lhs->u.varp;
    lhs_num = (MCINT) (lhs_var->var_index + 1);
    if (Is_Frmleqn(eqnp)) {
        type = (Is_Impleqn(eqnp)) ? 'M' : 'B';
    } else {
        type = (Is_Impleqn(eqnp)) ? 'N' : 'I';
    }

#define Is_Frmleqn(e)   (e)->eqtype & 1
#define Is_Impleqn(e)   (e)->eqtype & 2
    FNAME(add_eqname)(&eq_index, &name_len, (FUCHAR *) sp->name, &lhs_num, 
                      (FUCHAR *) &type);
    return NXTSYM;
}

/* count variables, parameters and user functions */
static int countVarPars(Symbol *sp) {
    Param *p;
    Variable *vp;

    if (sp->xpctype == XP_ENDO || sp->xpctype == XP_EXO ) {
        varCount++;
        varCharCount += strlen(sp->name);
        vp = sp->u.varp;
        if (vp->vtype == ENDO_FRML) {
            caCount++;
        }
        if (vp->maxlead > 0) {
            leadVarCount++;
        }
    } else if (sp->xpctype == XP_PARAM) {
        parCount++;
        parCharCount += strlen(sp->name);
        p = sp->u.parp;
        coefCount += p->cnt;
    } else if (sp->xpctype == XP_FUNC) {
        funCount++;
        funCharCount += strlen(sp->name);
    } else if (sp->xpctype == XP_ULFUNC) {
        ulFunCount++;
        ulFunCharCount += strlen(sp->name);
    }
    return NXTSYM;
}

static int addVarParName(Symbol *sp) {

    MCINT idx, arg_count, name_len;
    char type;
    Variable *vp;
    Param *p;
    Funcdef *f;

    if (sp->xpctype == XP_ENDO || sp->xpctype == XP_EXO ) {
        vp = sp->u.varp;
        idx = (MCINT) (vp->var_index + 1);
        type = vp->vtype;
        name_len = strlen(sp->name);
        switch (vp->vtype) {
            case EXO        : type = 'E'; break;
            case ENDO_IDENT : type = 'I'; break;
            case ENDO_FRML  : type = 'B'; break;
        }
        FNAME(add_varname)(&idx, &name_len, (FUCHAR *) sp->name, 
                          (FUCHAR *) &type);
    } else if (sp->xpctype == XP_PARAM) {
        p = sp->u.parp;
        idx = (MCINT) (p->par_index + 1);
        name_len = (FINT) strlen(sp->name);
        FNAME(add_parname)(&idx, &name_len, (FUCHAR *) sp->name);
    } else if (sp->xpctype == XP_FUNC) {
        f = sp->u.funp;
        idx = (MCINT) (f->func_index + 1);
        name_len = (FINT) strlen(sp->name);
        arg_count = (MCINT) f->argcnt;
        FNAME(add_funcname)(&idx, &name_len, (FUCHAR *) sp->name, &arg_count); 
    } else if (sp->xpctype == XP_ULFUNC) {
        f = sp->u.funp;
        idx = (MCINT) (f->func_index + 1);
        name_len = (FINT) strlen(sp->name);
        arg_count = (MCINT) f->argcnt;
        FNAME(add_ulfuncname)(&idx, &name_len, (FUCHAR *) sp->name); 
    }

    return NXTSYM;
}

void export_symtab() {

/*  count size of arrays */
    varCount = 0;
    varCharCount = 0;
    eqCount = 0;
    eqCharCount = 0;
    parCount = 0;
    parCharCount = 0;
    coefCount = 0;
    funCount = 0;
    funCharCount = 0;
    ulFunCount = 0;
    ulFunCharCount = 0;
    caCount = 0;
    leadVarCount = 0;

    sym_walk(Eqntp, countEquations, NULL );
    sym_walk(Stp, countVarPars, NULL );

    int error;
    FNAME(mc_alloc)(&eqCount, &varCount, &parCount, &coefCount,
                    &funCount, &ulFunCount,
                    &eqCharCount, &varCharCount, &parCharCount,
                    &funCharCount, &ulFunCharCount, &caCount, &leadVarCount,
                    &error);
    if (error == 0) {
        sym_walk(Eqntp, addEquationName, NULL );
        sym_walk(Stp, addVarParName, NULL );
    }
}
