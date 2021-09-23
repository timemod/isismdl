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

/* Alternative string comparison (assuming ASCII ordering) *
 * Returns zero, a positive or negative number, depending on the ordering 
 * of two strings.
 * This implementation is not entirely case insensitve:
 * If the only difference between the two strings is the case,
 * the function returns the result of the normal case sensensitive
 * comparison. This is needed because the isismdl variables names IS
 * case sensitive (the ordering is used to find model variables) */
int strcmp_alt(char const *a, char const *b, int nb) {

    // first do case insensitive comparison
    int i;
    for (i = 0; i < nb; i++) {
        if (!*(a + i) || !*(b + i)) {
           break;
        }
        /* for case insensitive comparison, 
         * convert to lower case, because in the ASCII ordering
         * the underscore comes before the underscore */
        int d = tolower(*(a + i)) - tolower(*(b + i));
        if (d != 0) return d;
    }

    // The case insensitive comparison did not detect any differences. 
    // Now use normal case sensitive comparison
    return strncmp(a, b, nb);
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
        // TODO: try strcoll (on Linux), this function takes the locale intp
        // acount. It turns out that the laxo model does not work with strcoll ordering.
        // The question is why. Could it be that something is 
        // wrong with the binary search algorithm? Check this.
        r = strcmp_alt((char *) str1 + *fb1 - 1, (char *) str2 + *fb2 - 1, *nb);
    }
    if (r) {
        return r > 0 ? +1 : -1;
    } else {
        return 0;
    }
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
    printf("result %d\n", strcmp_alt("aap", "noot", 3));
    printf("result %d\n", strcmp_alt("noot", "a", 1));
    printf("result %d\n", strcmp_alt("A", "a", 1));
    printf("result %d\n", strcmp_alt("a", "A", 1));
    printf("result %d\n", strcmp_alt("ab", "a_", 2));
    printf("result %d\n", strcmp_alt("a_", "ab", 2));
    printf("result %d\n", strcmp_alt("A", "a", 1));
}
#endif
