/* stricmp, strnicmp:  Case-insensitive versions of strcmp() and
 *                     strncmp().
 */

#include <ctype.h>

int stricmp(const char *s1, const char *s2) {
    register int c;

    while ((c = tolower(*s1)) == tolower(*s2)) {
        if (c == 0)
            return 0;
        s1++;
        s2++;
    }
    if (c < tolower(*s2))
        return -1;
    return 1;
}
