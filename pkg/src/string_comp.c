#include <R.h>
#include <Rinternals.h>
#include "isismdl_types.h"
#include <string.h>
#include <ctype.h>

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
 
    /* TODO: try strcoll (on Linux), tis function takes the locale into
     * acount. It turns out that the laxo model does not work with strcoll ordering.
     * The question is why. Could it be that something is 
     * wrong with the binary search algorithm? Check this.
    */
    int d = 0;
    int nb = *nb1 < *nb2 ? *nb1 : *nb2;

    if (nb == 0) return 0;

    /* first try case insensitive comparison */
    d = strncicmp((char *) str1 + *fb1 - 1, (char *) str2 + *fb2 - 1, nb);

    if (d == 0) {
        /* No case-insensitive difference in first nb bytes.
         * Compare lengths and if the strings have equal length do a case 
         * sensitive comparison */
        if (*nb1 == *nb2) {
            /* case sensitive comparison */
            d = strncmp((char *) str1 + *fb1 - 1, (char *) str2 + *fb2 -1, nb);
        } else {
            return *nb1 > *nb2 ? +1 : -1;
        }
    }
   
    if (d) {
        return d > 0 ? +1 : -1;
    } else {
        return 0;
    }
}

/* Case insensitive string comparison using ASCII ordering.
 * This function compares nb bites of string a and b.
 * These strings should not contain and end of string character ('\0').
 * TODO: on Linux, we might use strncasecmp, maybe something similar 
 *       is available on Windows.
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

