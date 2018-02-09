#include <Rinternals.h>
#include <Rdefines.h>

#define CHECK_LENGTH(name, value) if (length(value) > 1) { \
    error("The value for option %s should have length 1.", name); \
}

int get_non_negative_int(const char *name, SEXP value);
double get_finite_number(const char *name, SEXP value);
double get_positive_number(const char *name, SEXP value);
int get_logical(const char *name, SEXP value);
