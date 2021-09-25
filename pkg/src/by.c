#include <R.h>
#include <Rinternals.h>
#include "isismdl_types.h"
#include <string.h>
#include <ctype.h>

/*
 * R = bysget(str,fb)
 * return byte fb from str(1...)
 *
 */

int F77_SUB(bysget)(char *str, int *fb) {
    return (int) (*(str+*fb-1));
}

/*
 * R = bysset(str,fb)
 * set byte fb in str(1...) to bv
 *
 */

void F77_SUB(bysset)(FUCHAR *str, FINT *fb, FUINT *bv)
{
    *(str+*fb-1) = (FUCHAR)(*bv);
}

/*
 * call bysmov(fstr,fb,nb,tostr,tfb)
 * move nb bytes from fstr starting in fb to tostr starting at tfb
 *
 * use Standard C library routime memmove
 *  takes care of overlapping memory and should be fast
 */

void F77_SUB(bysmov)(FUCHAR *from, FINT *fb, FINT *nb, FUCHAR *to, FUINT *tofb)
{
    memmove(to+*tofb-1,from+*fb-1,*nb);
}

/*     BYteS ZERo initialize NB bytes in STR starting in byte FB to
 *     byte value 0.
 */

void F77_SUB(byszer)(FUCHAR *str, FINT *fb, FINT *nb)
{
    memset(str+*fb-1, 0, *nb);
}
