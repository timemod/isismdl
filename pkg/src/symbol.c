/*
 * symbol.c
 *
 * manage symbol table for xpc
 *
 * implemented as hash table
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "symbol.h"
#include "util.h"
#include "xpcdef.h"
#include "isismdl.h"

#define Namecmp strlcmp

#if 1

/*
 * pjw hash function
 */

#define NUM_BITS    (sizeof(unsigned) * 8)
#define THREE_FOURTHS   (NUM_BITS * 3 / 4)
#define ONE_EIGHTH  (NUM_BITS / 8)
#define HIGH_BITS   (~ ((unsigned) ~0 >> ONE_EIGHTH))

static  unsigned hashf(char *s) {
    unsigned h, tmp;

    for (h = 0; *s; s++ ) {
        h = (h << ONE_EIGHTH) + *s;
        if( (tmp = h & HIGH_BITS) != 0 )
            h = (h ^ (tmp >> THREE_FOURTHS)) & ~ HIGH_BITS;
    }

    return h % HASHSIZE;
}
#endif

SymTab  *sym_init(void) {
        SymTab  *stab;
        size_t  i;

        stab = emalloc( sizeof(SymTab) );
        for( i = 0; i < HASHSIZE; i++ )
                stab->tab[i] = NULL;

        return stab;
}

Symbol *sym_tlookup(SymTab *stab, char *s, int t) {
    /* find s + type t in symbol table */

    Symbol  *sp;

    for( sp = stab->tab[hashf(s)]; sp != NULL; sp = sp->next )
    {
        if( (Namecmp(sp->name, s) == 0) && (sp->type == t) )
            return sp;
    }
    return NULL;
}

Symbol  *sym_lookup(SymTab *stab, char *s)
{
        /* find s in symbol table */

    Symbol  *sp;

    for( sp = stab->tab[hashf(s)]; sp != NULL; sp = sp->next )
    {
        if( Namecmp(sp->name, s) == 0 )
            return sp;
    }

    return NULL;       /* not found */
}

Symbol  *sym_install(SymTab *stab, char *s, int t )
{
        /* install s in symbol table */

    Symbol  *sp;
    unsigned hv;

    sp = (Symbol *) emalloc(sizeof(Symbol) + strlen(s) + 1);
    sp->name = (char *) sp + sizeof(*sp);
    strcpy(sp->name, s );
    hv      = hashf(s);
    sp->type    = t;
    sp->xpctype = XP_UNDEF;
    sp->next    = stab->tab[hv];    /* put at front of list */
    stab->tab[hv] = sp;

    return sp;
}

Symbol *sym_create(SymTab *stab, char *s , int t) {
        Symbol  *sp;
        sp = sym_lookup( stab, s );
        if( sp == NULL )
                sp = sym_install( stab, s, t );

        return sp;
}

void    sym_uninstall( SymTab *stab, char *s )
{
        /* uninstall name *s from symbol table */

    Symbol  *sp, *prev;
    unsigned hv;


    hv = hashf(s);
    prev = NULL;
    for( sp = stab->tab[hv]; sp != NULL; sp = sp->next )
    {
        if( Namecmp(s, sp->name) == 0 )
            break;
        prev = sp;
    }

    if( sp != NULL )
    {
        if( prev != NULL )
            prev->next = sp->next;
        else
            stab->tab[hv] = sp->next;      /* at head of list */

        efree(sp);
    }
}

Symbol  *sym_unlink( SymTab *stab, char *s )
{
        /* unlink name *s from symbol table */

    Symbol  *sp, *prev;
    unsigned hv;

    hv = hashf(s);
    prev = NULL;
    for( sp = stab->tab[hv]; sp != NULL; sp = sp->next )
    {
        if( Namecmp(s, sp->name) == 0 )
            break;
        prev = sp;
    }

    if( sp != NULL )
    {
        if( prev != NULL )
            prev->next = sp->next;
        else
            stab->tab[hv] = sp->next;      /* at head of list */
    }
        return sp;
}

void    sym_walk( SymTab *stab, int (*walkfn)(Symbol *) , Symbol **spsave )
{
    Symbol  *sp, *prev, *p;
    size_t  i;

    if( spsave != NULL )
        *spsave = NULL;

    for( i = 0; i < HASHSIZE; i++ )
    {
        for( prev = NULL, sp = stab->tab[i]; sp != NULL ; )
        {
            switch( (*walkfn)(sp) )
            {
                case NXTSYM:    /* goto next symbol */
                    prev = sp;
                    sp = sp->next;
                    break;

                case DELSYM:    /* delete current symbol */
                    if(prev != NULL)
                        prev->next = sp->next;
                    else        /* at head of list */
                        stab->tab[i] = sp->next;

                    p = sp->next;
                    free( sp );
                    sp = p;
                    break;

                case LWQUIT:    /* save symbol pointer for caller */
                    if( spsave != NULL )
                        *spsave = sp;
                    return;
            }
        }
    }
}


static int print_symbol(Symbol *sp) {
    PRINTF("Name and type = %s %d\n", sp->name, sp->xpctype);
    return NXTSYM;
}

void sym_dump(SymTab *stab) {
    PRINTF("Dump of the symbol table\n");
    sym_walk(stab, print_symbol,  NULL);
}

void sym_stat( SymTab *stab, FILE *fp ) {
    Symbol  *sp;
    size_t  i, cnt, zcnt, cntmax, totcnt;

    zcnt = cntmax = totcnt = 0;

    for (i = 0; i < HASHSIZE; i++ ) {
        cnt = 0;
        for (sp = stab->tab[i]; sp != NULL ; sp = sp->next) {
            ++cnt;
        }
        if (cnt == 0) {
             zcnt++;
        } else if (cnt > cntmax) {
             cntmax = cnt;
        }
        if (cnt) {
             totcnt += cnt;
        }
    }

    fprintf(fp, "%lu zero length chains, ", (unsigned long int) zcnt );
    fprintf(fp, "longest chain %lu, ", (unsigned long int) cntmax );
    fprintf(fp, "average non zero chain length %lu\n",
            (unsigned long int) (totcnt / (HASHSIZE - zcnt)));
}

static int delete_symbol(Symbol *sp)
{
    Equation *eqnp;
    Variable *vp;
    Param *p;
    Funcdef *fp;
    int i;

    if (sp->xpctype == XP_EQN && (eqnp = sp->u.eqnp) != NULL)  {

        free(eqnp);

    } else if (   (sp->xpctype == XP_ENDO || sp->xpctype == XP_EXO) 
             && (vp = sp->u.varp) != NULL)  {

        free(vp);

    } else if (sp->xpctype == XP_PARAM && (p = sp->u.parp) != NULL) {

        free(p);

    } else if (sp->xpctype == XP_FUNC && (fp = sp->u.funp) != NULL)  {

        /* free argument names */
        if (fp->argnames) {
            for (i = 0; i < fp->argcnt; i++) {
                free(fp->argnames[i]);
            } 
            free(fp->argnames);
        }

        /* free argdefs */
        if (fp->argdp) {
            for (i = 0; i < fp->argcnt; i++) {
                free(fp->argdp[i]);
            } 
            free(fp->argdp);
        }

        /* free funcdef */
        free(fp);

    }

    return DELSYM;
}


void free_symtab(SymTab *stab) 
    /*
     * free memory associated with the symbol table 
     */
{
     sym_walk(stab, delete_symbol, NULL);
     free(stab);
}

