#include "set_option_utils.h"

int get_non_negative_int(const char *name, SEXP value) {
    /* INPUT:
     * name  : the name of the option
     * value : the value of the options as specified to the user
     *
     * This function converts value to an int, and gives
     * an error if value is not a non-negative integer.
     */

    const char *msg = "%s should be an integer";
    if (!isNumeric(value)) {
        error(msg, name); 
    }
    int i = asInteger(value);
    if (i == NA_INTEGER) {
        error("%s should not be NA", name); 
    } else if (i < 0) {
        error("%s should be a non-negative integer", name); 
    } 
    if (!isInteger(value)) {
        if (i != asReal(value)) {
            error(msg, name); 
        }
    }
    return i;
}

double get_finite_number(const char *name, SEXP value) {
    /* INPUT:
     * name  : the name of the option
     * value : the value of the options as specified to the user
     *
     * This function converts value to a double , and gives
     * an error if value is not a finite number (finite means:
     * not NA or Inf etc.)
     */
    const char *msg = "%s should be a number";
    if (!isNumeric(value)) {
        error(msg, name); 
    }
    double x = asReal(value);
    if (!R_FINITE(x)) {
        error("%s should  be a finite number", name); 
    }
    return x;
}

double get_positive_number(const char *name, SEXP value) {
    /* INPUT:
     * name  : the name of the option
     * value : the value of the options as specified to the user
     *
     * This function converts value to a double , and gives
     * an error if value is not a positive finite number (finite means:
     * not NA or Inf etc.)
     */
    double x = get_finite_number(name, value);
    if (x <= 0) {
        error("%s should  be a positive number", name); 
    }
    return x;
}

