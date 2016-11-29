/*
 * R = byscmp(str1,fb1,nb,str2,fb2)
 * compare <nb> bytes str(fb1..) and str2(fb2..)
 *
 * R = bysfcp(str1,nb,str2)
 * compare <nb> bytes str(1) and str2(1)
 *
 * both functions return
 *
 *  return   0 if all equal
 *          -1 id str1(k) < str2(k)
 *          +1 id str1(k) > str2(k)
 *
 * use Standard C library routime memcmp
 */

#include "isismdl_types.h"

#include <string.h>
#include <stdio.h>

FINT FNAME(byscmp)(FUCHAR *str1, FINT *fb1, FINT *nb, FUCHAR *str2, FINT *fb2) {
    int r = 0;
    if (*nb > 0) {
        r = memcmp(str1+*fb1-1, str2+*fb2-1, *nb);
    }
    if (r) {
        return r > 0 ? +1 : -1;
    } else {
        return 0;
    }
}

FINT FNAME(bysfcp)(FUCHAR *str1, FINT *nb, FUCHAR *str2)
{
    int r = 0;

    if( *nb > 0 )
        r = memcmp(str1, str2, *nb);

    if( r )
        return r > 0 ? +1 : -1;
    else
        return 0;
}
