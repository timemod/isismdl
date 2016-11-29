/*
 * R = bysget(str,fb)
 * return byte fb from str(1...)
 *
 */

#include <R.h>
#include <Rinternals.h>

int F77_SUB(bysget)(char *str, int *fb) {
    return (int) (*(str+*fb-1));
}
