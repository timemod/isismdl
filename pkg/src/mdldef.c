/* This code is used by the model conversion utils */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "symbol.h"
#include "mdldef.h"
#include "xpcdef.h"
#include "util.h"
#include "mcinit.h"

/*
 * Save Symbol table pointers for equations
 * called from yyparse
 * in the order that equations have been parsed
 * so that we can walk through the equations
 * in the order that they have been defined
 *
 * Do same for parameters and variables
 */

Symbol  **eqnp  = NULL;
size_t  ecnt    = 0;
size_t  ecntmax = 0;

#define ECNTSIZ 100

Symbol  **parp  = NULL;
size_t  pcnt    = 0;
size_t  pcntmax = 0;

size_t  maxpnamelen = 0;

#define PCNTSIZ 25

size_t vcnt    = 0;
size_t vcntmax = 0;

Symbol **varp = NULL;


size_t endocnt = 0;
size_t exocnt  = 0;
size_t maxvnamelen = 0;

#define VCNTSIZ 500


void new_eqn( Symbol *sp )
{
    if( ecnt >= ecntmax )
    {
            eqnp = erealloc( eqnp, (ecntmax + ECNTSIZ) * sizeof(Symbol*) );
            ecntmax += ECNTSIZ;
    }

    eqnp[ecnt++] = sp;
}

void new_par( Symbol *sp )
{
    if( pcnt >= pcntmax )
    {
            parp = erealloc( parp, (pcntmax + PCNTSIZ) * sizeof(Symbol*) );
            pcntmax += PCNTSIZ;
    }

    parp[pcnt++] = sp;
    maxpnamelen = max(strlen(sp->name), maxpnamelen);
}

static void new_var( Symbol *sp )
{
    if( vcnt >= vcntmax )
    {
            varp = erealloc( varp, (vcntmax + VCNTSIZ) * sizeof(Symbol*) );
            vcntmax += VCNTSIZ;
    }

    varp[vcnt++] = sp;
}

static int collectvar( Symbol *sp )
{
    /*
     * collect variables in array
     * for later sorting
     */

    if( sp->xpctype == XP_ENDO || sp->xpctype == XP_EXO )
    {
        new_var(sp);

        if( sp->xpctype == XP_ENDO )
            ++endocnt;
        else
            ++exocnt;

        maxvnamelen = max(strlen(sp->name), maxvnamelen);
    }

    return NXTSYM;
}

/*
 * name comparison function for qsort
 */

static int varcmp( const void *v1, const void *v2)
{
    Symbol *sp1 = *(Symbol **) v1;
    Symbol *sp2 = *(Symbol **) v2;

    int k;

    k = strlcmp(sp1->name, sp2->name);

#if DBG
    printf("sp1->name=%s, sp2->name=%s, k=%d\n",sp1->name,sp2->name,k);
#endif

    return  k;
}

void mkvarlist()
{
    sym_walk(Stp, &collectvar, NULL );
#if DBG
    printf("Before qsort: [0]     :%s\n", varp[0]->name);
    printf("Before qsort: [vcnt-1]:%s\n", varp[vcnt-1]->name);
#endif
    qsort( &varp[0], vcnt, sizeof(Symbol *), &varcmp);

#if DBG
    printf("After  qsort: [0]     :%s\n", varp[0]->name);
    printf("After  qsort: [vcnt-1]:%s\n", varp[vcnt-1]->name);
#endif
}
