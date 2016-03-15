/*
 * Filename utilities 
 * These functions should only be called from other C functions
 */

#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/stat.h>

#include "futils.h"

#ifdef OS_WINDOWS
#define DPSEP "\\/:"
#else
#define DPSEP "/"
#endif

size_t get_parent_dir(char *path, char *fname) 
{
    /*
     * Determines the name of the parent directory
     * of the specified file or directory. The result is
     * stored in argument path. The return value is the
     * length of the path excluding terminating '0'.
     */

     char *pstart, *bstart, *p;
     size_t n;

     pstart = bstart = p = fname;

     while( (p = strpbrk(p, DPSEP)) != NULL )
         bstart = ++p;


     if (bstart > pstart) {
         n = bstart - pstart - 1;
     } else {
         n = 0;
     }

     if (n > 0) {
         memcpy(path, fname, n);
     }
     path[n] = '\0';

     return n;
}


int file_exists (char *filename) {

    /* file_exists returns 1 when the specified file or directory
     * exists, and 0 otherwise.
     */

    struct stat buf;
    int i = stat ( filename, &buf ); 
    /* File found */
    if ( i == 0 ) { 
        return 1; 
    } 
    return 0;   
}
