/*
 * call bysmov(fstr,fb,nb,tostr,tfb)
 * move nb bytes from fstr starting in fb to tostr starting at tfb
 *
 * use Standard C library routime memmove
 *  takes care of overlapping memory and should be fast
 */

#include "macromodtypes.h"

#include <string.h>

void FNAME(bysmov)(FUCHAR *from, FINT *fb, FINT *nb, FUCHAR *to, FUINT *tofb)
{
    memmove(to+*tofb-1,from+*fb-1,*nb);
}
