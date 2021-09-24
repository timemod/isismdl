#include <R.h>
#include <Rinternals.h>
#include "isismdl_types.h"
#include <string.h>
#include <ctype.h>

/* Do not use strcoll for string comparison, for two reasons:
 * 1) The ordering is platform dependent and thefore 
 *    different on Windows and Linux.
 * 2) For the default locale on Linux (en_US.utf8), strcoll procedures
 *    a strange ordering (when string are compared, underscores are ignorted).
 * Note: the ordering has an effect on order of the names in the mrf file
 */
//#define USE_STRCOLL

static int strncicmp(char const *a, char const *b, int nb);

/*
 * R = string_comp(str1, fb1, nb1, str2, fb2, nb2)
 * compares string str1 and str2 lexicographically
 * fb1 is the start index in string str2
 *
 *  return   0 if all equal
 *          -1 id str1 < str2
 *          +1 id str1 > str2
 */

int F77_SUB(string_comp)(FUCHAR *str1, FINT *fb1, FINT *nb1, 
                         FUCHAR *str2, FINT *fb2, FINT *nb2) {
 
    int d = 0;

#ifdef USE_STRCOLL

    char s1[MCMXNM + 1];
    char s2[MCMXNM + 1];
    memcpy(s1, str1 + *fb1 - 1, *nb1);
    memcpy(s2, str2 + *fb2 - 1, *nb2);
    s1[*nb1] = '\0';
    s2[*nb2] = '\0';
    d = strcoll(s1, s2);

#else 

    int nb = *nb1 < *nb2 ? *nb1 : *nb2;

    if (nb == 0) return 0;

    /* first try case insensitive comparison */
    d = strncicmp((char *) str1 + *fb1 - 1, (char *) str2 + *fb2 - 1, nb);

    if (d == 0) {
        /* No case insensitive difference in first nb bytes.
         * Compare lengths and if the strings have equal length do a case 
         * sensitive comparison */
        if (*nb1 == *nb2) {
            /* case sensitive comparison */
            d = strncmp((char *) str1 + *fb1 - 1, (char *) str2 + *fb2 -1, nb);
        } else {
            return *nb1 > *nb2 ? +1 : -1;
        }
    }
#endif
   
    if (d) {
        return d > 0 ? +1 : -1;
    } else {
        return 0;
    }
}


/* Case insensitive string comparison using ASCII ordering.
 * This function compares nb bites of string a and b.
 * These strings should not contain and end of string character ('\0').
 * Note that in principle standard function strncasecmp can be used.
 * However, according to the POSIX standard the behaviour is undefined
 * if the locale is not POSIX or C. Therefore use our own implementation.
 */
static int strncicmp(char const *a, char const *b, int nb) {

    int i;
    for (i = 0; i < nb; i++) {
        int d = tolower(*(a + i)) - tolower(*(b + i));
        if (d != 0) return d;
    }

    // no differences detected
    return 0;
}

