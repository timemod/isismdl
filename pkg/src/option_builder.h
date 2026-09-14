#ifndef GET_OPTION_UTILS_H
#define GET_OPTION_UTILS_H

#include <Rinternals.h>

typedef struct {
    SEXP options; /* protected in ob_create until ob_get */
    SEXP names;   /* protected in ob_create until ob_get */
    int cnt;
    int n;
} OptionBuilder;

OptionBuilder *ob_create(int n);
void ob_add_string(OptionBuilder *b, const char *name, const char *value);
void ob_add_sexp(OptionBuilder *b, const char *name, SEXP value);
void ob_add_int(OptionBuilder *b, const char *name, int value);
void ob_add_real(OptionBuilder *b, const char *name, double value);
void ob_add_logical(OptionBuilder *b, const char *name, int value);
SEXP ob_get(OptionBuilder *b);

#endif /* GET_OPTION_UTILS_H */

