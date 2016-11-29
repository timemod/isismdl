/*
 *      various utility functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>
#include <Rinternals.h>

#include "util.h"
#include "isismdl.h"

void *emalloc(size_t n ) {
        void    *p = malloc( n );

        if (p == NULL ) {
            ERROR( "Out of memory");
        }
        return p;
}

void  *erealloc( void *src, size_t n) {
        void    *p = realloc( src, n );

        if (p == NULL) {
            ERROR("Out of memory");
        }

        return p;
}

void efree(void *p) {
        if (p) {
            free(p);
        }
}

char *estrdup( char *s )
{
        char    *p;

        if( s == NULL )
                return s;

        p = emalloc( strlen(s) + 1 );

        strcpy(p, s);
        return p;
}

FILE *efopen( const char *fname, char *amode) {
        FILE    *fp;

        if( (fp = fopen(fname, amode)) == NULL) {
            ERROR("can't open %s\n", fname);
        }
        return fp;
}


/*
 * strlcmp:  Case-insensitive versions of strcmp()
 */

int strlcmp(const char *s1, const char *s2)
{
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

/*
 *  getoptions
 *
 *  Parse the command line options, System V style.
 *
 *  Standard option syntax is:
 *
 *  option ::= SW [optLetter]* [argLetter space* argument]
 *
 *  where
 *
 *  - there is no space before any optLetter or argLetter.
 *  - opt/arg letters are alphabetic, not punctuation
 *    characters.
 *  - optLetters, if present, must be matched in optstr.
 *  - argLetters, if present, are found in optstr
 *    followed by ':'.
 *  - argument is any white-space delimited string.
 *    Note that it can include the SW character.
 *  - upper and lower case letters are distinct.
 *
 *  If no arguments are left then getoptions() returns EOF.
 *  The sequence "--" always ends an argument list.
 *
 *  The argument <optstr> to getoptions() describes the valid
 *  arguments (see also above ).
 *
 *  The global <optind> starts at 1 and is always left
 *  as the index of the next argument of argv[].
 *
 *  The global <optarg> is set to point at the string
 *  following an argletter; if none is following then
 *  optarg == NULL.
 *
 *  The global <optlet> is set to the current option
 *  letter found on the command line.
 *
 *  If an error occurs then getoptions() prints an error
 *  message (if opterr != 0) and returns '?'.
 *
 */

/*  externally accessible variables                 */

int optind  = 1;    /* index of next argument           */
char   *optarg;     /* pointer to argument of current option    */
int optlet;     /* current option letter found          */

int getoptions(int argc, char *argv[], char *optstr)
{
    int c;
    static int sp = 1;
    char    *ap;

    if( sp == 1 )
    {
        if( optind >= argc || argv[optind][0] != '-' )
            return(EOF) ;       /* not an option */

        else if( strcmp( argv[optind], "--") == 0 ) /* -- */
        {
            optind++;
            return(EOF);
        }
    }

    c = optlet = argv[optind][sp++];

    if( c == ':' || (ap = strchr(optstr, c) ) == NULL )
    {
        if( !argv[optind][sp] ) /* error */
        {
            optind++;
            sp = 1;
        }

        return( '?' );
    }
    else if( *++ap == ':' )
    {
        if( argv[optind][sp] )
        {
            optarg = &argv[optind++][sp];
            sp = 1;
        }
        else if( ++optind < argc )
        {
            optarg = &argv[optind++][0];
            sp = 1;
        }
        else                /* error */
        {
            optarg = NULL;
            sp = 1;
            c = ':';
        }

        return c;
    }
    else
    {
        optarg = NULL;

        if( !argv[optind][sp] )
        {
            optind++;
            sp = 1;
        }
    }

    return c;
}
