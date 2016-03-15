/*
 * initialize symbol table and entries for predefined symbols
 * for the model compiler
 */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "symbol.h"
#include "mcinit.h"
#include "util.h"
#include "xpcdef.h"
#include "ecode.h"
#include "xpctab.h"

/*
 * Keywords of the model language
 *
 * Stp      main symbol table for builtin, command and variables
 * Eqntp    equation name symbol table
 *
 */

SymTab  *Stp   = NULL;
SymTab  *Eqntp = NULL;

static struct keywords
{
    char    *name;
    int     toktype;
    int     xpopcode;
}

kw[] =
{
    {"abs"           , T_BUILTIN     , XP_BABS },
    {"atan"          , T_BUILTIN     , XP_BATAN},
    {"const"         , T_PARAM       , XP_CMD  },
    {"cos"           , T_BUILTIN     , XP_BCOS },
    {"del"           , T_DEL         , XP_CMD  },
    {"else"          , T_ELSE        , XP_CMD  },
    {"end"           , T_END         , XP_CMD  },
    {"elseif"        , T_ELSEIF      , XP_CMD  },
    {"endif"         , T_ENDIF       , XP_CMD  },
    {"exp"           , T_BUILTIN     , XP_BEXP },
    {"frml"          , T_FRML        , XP_CMD  },
    {"function"      , T_FUNCTION    , XP_CMD  },
    {"ident"         , T_IDENT       , XP_CMD  },
    {"if"            , T_IF          , XP_CMD  },
    {"log"           , T_BUILTIN     , XP_BLOG },
    {"log10"         , T_BUILTIN     , XP_BLOG10},
    {"min"           , T_BUILTIN     , XP_BMIN },
    {"max"           , T_BUILTIN     , XP_BMAX },
    {"nint"          , T_BUILTIN     , XP_BNINT},
    {"param"         , T_PARAM       , XP_CMD  },
    {"sin"           , T_BUILTIN     , XP_BSIN },
    {"tan"           , T_BUILTIN     , XP_BTAN },
    {"sqrt"          , T_BUILTIN     , XP_BSQRT},
    {"asin"          , T_BUILTIN     , XP_BASIN},
    {"acos"          , T_BUILTIN     , XP_BACOS},
    {"sinh"          , T_BUILTIN     , XP_BSINH},
    {"cosh"          , T_BUILTIN     , XP_BCOSH},
    {"tanh"          , T_BUILTIN     , XP_BTANH},
    {"toreal"        , T_BUILTIN     , XP_BTOREAL},
    {"cumnor"        , T_BUILTIN     , XP_BCUMNOR},
    {"invnor"        , T_BUILTIN     , XP_BINVNOR},
    {"hypot"         , T_BUILTIN     , XP_BHYPOT},
    {"fibur"         , T_BUILTIN     , XP_BFIBUR},
    {"sum"           , T_SUM         , XP_CMD  },
    {"then"          , T_THEN        , XP_CMD  },
    {"ul_function"   , T_UL_FUNCTION , XP_CMD  }
};

#define NBR_KWENTRIES ( sizeof(kw) / sizeof(kw[0]) )

void mcinit(void) {
    int     i;
    Symbol  *sp;

    /* 
     * If a symbol table already exists (mcexec is called more than once),
     * then free all the memory for the old symbol table before creating 
     * a new symbol table
     */
    if (Stp != NULL) {
        free_symtab(Stp);
    }
    Stp = sym_init();

    for( i = 0 ; i < NBR_KWENTRIES; i++ )
    {
        sp = sym_install( Stp, kw[i].name, kw[i].toktype);
        sp->xpctype = kw[i].xpopcode;
        sp->u.handle = i;
    }

    if (Eqntp != NULL) {
        free_symtab(Eqntp);
    }
    Eqntp = sym_init();
}
