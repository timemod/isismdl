
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

/* case insensitive comparison.*/
int strcicmp(char const *a, char const *b, int nb) {
    int i;
    for (i = 0 ; i < nb; i++) {
        /* convert to upper case, because in the ASCII ordering
         * the upper case letters come before the underscore */
        int d = tolower(*(a + i)) - tolower(*(b + i));
        if (d == 0) {
            /* same letter, in that upper case before upper case */
            d = *(a + i) - *(b + i);
        }
        if (d != 0 || !*(a + 1) || !*(b + 1)) {
            return d;
        }
    }
}

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

int F77_SUB(byscmp)(FUCHAR *str1, FINT *fb1, FINT *nb, FUCHAR *str2, FINT *fb2) {
    int r = 0;
    if (*nb > 0) {
        r = strcicmp(str1+*fb1-1, str2+*fb2-1, *nb);
    }
    if (r) {
        return r > 0 ? +1 : -1;
    } else {
        return 0;
    }
}

int F77_SUB(bysfcp)(FUCHAR *str1, FINT *nb, FUCHAR *str2)
{
    int r = 0;

    if( *nb > 0 )
        r = memcmp(str1, str2, *nb);

    if( r )
        return r > 0 ? +1 : -1;
    else
        return 0;
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

#ifdef TEST
#include <stdio.h>

int main() {
    printf("result %d\n", strcicmp("aap", "noot", 3));
    printf("result %d\n", strcicmp("noot", "a", 1));
    printf("result %d\n", strcicmp("A", "a", 1));
    printf("result %d\n", strcicmp("a", "A", 1));
    printf("result %d\n", strcicmp("ab", "a_", 2));
    printf("result %d\n", strcicmp("a_", "ab", 2));
}
#endif
