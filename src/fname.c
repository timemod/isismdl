/*
 * split a filename into path base ext
 * doesn't handle faulty names very well
 * ..z is put into extension part
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "fname.h"

#define DPSEP "\\/:"
#define EXSEP '.'

void    fnsplit( char *fname, char *path, char *base, char *ext)
{
        char    *pstart, *bstart, *estart, *p;
        size_t  n;

        bstart = pstart = p = fname;

        while( (p = strpbrk(p, DPSEP)) != NULL )
                bstart = ++p;

        /*
         * pstart : path
         * bstart : basename
         */

        if( bstart == pstart )
                *path = '\0';
        else
        {
                memcpy( path, pstart, n = bstart - pstart );
                path[n] = '\0';
        }

        estart = strchr( bstart, EXSEP );

        if( estart )
        {
                memcpy( base, bstart, n = estart - bstart );
                base[n] = '\0';
                strcpy( ext, estart );
        }
        else
        {
                strcpy( base, bstart );
                *ext = '\0';
        }

}

char *get_path_name(char *fname) 
{
    /*
     * Returns the path name for the specified file name 
     */

     char *pstart, *bstart, *p, *path_name;
     size_t  n;

     pstart = bstart = p = fname;

     while( (p = strpbrk(p, DPSEP)) != NULL )
         bstart = ++p;

     n = bstart - pstart;
     path_name = malloc(n + 1);

     if (n > 0) {
         memcpy( path_name, fname, n);
     }
     path_name[n] = '\0';

     return path_name;
}

char    *fnjoin ( char *fname, char *path, char *base, char *ext)
{
        size_t  plen = strlen(path);
        size_t  blen = strlen(base);
        size_t  elen = strlen(ext );
        int     dflag= 0;
        char    *p;

        if( *ext && *ext != EXSEP )
                dflag = 1;

        if( plen + blen + elen + dflag >= FILENAME_MAX )
                return NULL;

        p = fname;
        strcpy( p , path ); p += plen;
        strcpy( p , base ); p += blen;

        if( dflag )
                *p++ = EXSEP;

        strcpy( p , ext  );

        return fname;
}
