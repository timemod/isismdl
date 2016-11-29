/*
 * Filename utilities 
 * These functions should only be called from other C functions
 */

#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/stat.h>

#include "futils.h"

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
