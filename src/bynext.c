/*
 * bynext routines
 *
 *  bysgti  initialize get
 *  bysgtu  update get
 *  bysgtr  reset get
 *
 *  byssti  initialize set
 *  bysstu  update set
 *  bysstr  reset set
 *
 *  bysgtn
 *  bysgn2
 *  byign2
 *
 *  bysstn
 *  byslkn
 */

#include <stdio.h>

#include "macromodtypes.h"

#define max_depth 6
#define stack_size max_depth

/*
 * data structure for GET
 */

struct bygn_
{
    unsigned char *adrssg;
    long int shftg;
} BYGN;

struct
{
  struct bygn_ *gptr;
  struct bygn_ gtnbl[stack_size];
} BYZGTN;

/*
 * data structure for SET
 */

struct bysn_
{
  unsigned char *adrsss;
  long int shfts;
} BYSN;

struct
{
  struct bysn_ *sptr;
  struct bysn_ stnbl[stack_size];
} BYZSTN;


void FNAME(bysgti)(FUCHAR *str, FINT *fb)
{
    /* test if gptr is initialized */

    if (BYZGTN.gptr == NULL)
        BYZGTN.gptr = BYZGTN.gtnbl;

    /* push old common block */

    BYZGTN.gptr->adrssg = BYGN.adrssg;
    BYZGTN.gptr->shftg  = BYGN.shftg;
    BYZGTN.gptr++;

    /* initialize common block */

    BYGN.adrssg = str;
    BYGN.shftg  = *fb-1;
}

void FNAME(bysgtu)(FINT *nb)
{
    BYGN.shftg += *nb;
}

void FNAME(bysgtr)(void)
{
    /* pop old common block */

    --BYZGTN.gptr;

    BYGN.adrssg = BYZGTN.gptr->adrssg;
    BYGN.shftg  = BYZGTN.gptr->shftg;
}


FINT FNAME(bysgtn)(void)
{
    return (FINT) BYGN.adrssg[BYGN.shftg++];
}

FINT FNAME(byslkn)(void)
{
    return (FINT) BYGN.adrssg[BYGN.shftg];
}

FINT FNAME(bysgn2)(void)
{
    FUCHAR b1,b2;

    b1 = BYGN.adrssg[BYGN.shftg];
    b2 = BYGN.adrssg[BYGN.shftg+1];

    BYGN.shftg += 2;
    return (FINT) (b1 | (b2<<8));
}

FINT FNAME(byign2)(void)
{
    FUCHAR b1,b2;
    short k;

    b1 = BYGN.adrssg[BYGN.shftg];
    b2 = BYGN.adrssg[BYGN.shftg+1];

    k = (b1 | (b2<<8));

    BYGN.shftg += 2;
    return (FINT) k;
}


/*  Set routines */

void FNAME(byssti)(FUCHAR *str, FINT *fb)
{
    /* test if sptr is initialized */

    if (BYZSTN.sptr == NULL)
        BYZSTN.sptr = BYZSTN.stnbl;

    /* push old common block */

    BYZSTN.sptr->adrsss = BYSN.adrsss;
    BYZSTN.sptr->shfts  = BYSN.shfts;
    BYZSTN.sptr++;

    /* initialize common block */

    BYSN.adrsss = str;
    BYSN.shfts  = *fb-1;
}

void FNAME(bysstu)(FINT *nb)
{
    BYSN.shfts += *nb;
}

void FNAME(bysstr)(void)
{
    /* pop old common block */

    --BYZSTN.sptr;
    BYSN.adrsss = BYZSTN.sptr->adrsss;
    BYSN.shfts  = BYZSTN.sptr->shfts;
}

void FNAME(bysstn)(FINT *bvs)
{
    BYSN.adrsss[BYSN.shfts++] = (unsigned char)*bvs;
}
