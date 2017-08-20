/*
 * Output system for the output of modelcode (used for the model
 * conversion tools)
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "util.h"
#include "oprintf.h"
#include "isismdl.h"

#define MAXLEN  75

static  FILE *fout;
static  int olen   = 0; /* length of output line so far                 */
static  int leader = 0; /* # of blanks at start of continuation line    */
static  int sflag  = 0; /* 1 if current output line > MAXLEN            */
static  int lflag  = 0; /* 1 if printed more than 1 line                */
static  int maxowid= MAXLEN;

static  char linecontchar = 0;

static  void wrterr(void)
{
    ERROR( "write error on .oco file\n");
}

static void ovprintf( char *fmt, va_list ap )
{
    int x;
    int gotnl = 0;

    if(strcmp(fmt,"\n") == 0)
    {
        olen  = 0;
        sflag = 0;
        lflag = 1;
        gotnl = 1;
    }
    else if( sflag )
    {
        if( linecontchar == 0 )
            fprintf(fout, "\n%*s", leader, "" );
        else
            fprintf(fout, " %c\n%*s", linecontchar, leader, "" );

        olen = leader;
        sflag = 0;
        lflag = 1;
    }

    x = vfprintf(fout, fmt, ap);
    if( x < 0 )
        wrterr();

    olen += gotnl? 0 : x;
    if( olen > maxowid )
        sflag = 1;
}

void oprintf( char *fmt, ... )
{
    va_list ap;

    va_start(ap, fmt);
    ovprintf(fmt, ap);
    va_end(ap);
}

void xprintf( char *fmt, ... )
{
    int     x;
    va_list ap;

    va_start(ap, fmt);
    x = vfprintf(fout, fmt, ap);
    if( x < 0 )
        wrterr();
    va_end(ap);
}

void initout()
{
    olen  = sflag = lflag = leader = 0;
}

void setoleader()
{
    leader = olen;
}

void setmaxowid(size_t width)
{
    maxowid = width;
}

void setfpout(FILE *fp)
{
    fout = fp;
    initout();
}

void setlinecont(char x)
{
    linecontchar = x;
}
