/*
 *      prototypes for functions in util.c
 */

#ifndef UTIL_H
#define UTIL_H

void    *emalloc ( size_t );
void    *erealloc( void *, size_t );
void    *ecalloc ( size_t , size_t );

void    efree( void *p );
char    *estrdup( char *s );

FILE    *efopen( const char *fname, char *amode);

int strlcmp(const char *s1, const char *s2);

int getoptions(int argc, char *argv[], char *optstr);

    /* parse command line Unix System V style
     * see util.c for the details
     */

extern  int     optind; /* index of next argument        */
extern  int     optlet; /* current option letter found       */
extern  char    *optarg;/* pointer to argument of current option */

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))
#endif
