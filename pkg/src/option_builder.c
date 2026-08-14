#include <Rinternals.h>
#include "option_builder.h"

/* 
 * C code for creating a named list with options (solve or fit options).
 */

/* Create a option builder for n entries.
   The returned builder is allocated with R_alloc (auto-freed) and holds
   two PROTECTed objects (options and names). Call ob_get() to finalize
   and return the named list (ob_get will UNPROTECT them). */
OptionBuilder *ob_create(int n) {
    OptionBuilder *b = (OptionBuilder *) R_alloc(1, sizeof(OptionBuilder));
    b->n = n;
    b->cnt = 0;
    b->options = PROTECT(allocVector(VECSXP, n));
    b->names   = PROTECT(allocVector(STRSXP, n));
    return b;
}

/* Internal helper: ensure there is room, otherwise raise an R error. */
static void ob_check_space(OptionBuilder *b) {
    if (b->cnt >= b->n) {
        Rf_error("OptionBuilder overflow: max %d options", b->n);
    }
}

/* Add an arbitrary SEXP value. The SEXP may be unprotected by the caller because
   placing it into the protected options vector first makes it reachable. */
void ob_add_sexp(OptionBuilder *b, const char *name, SEXP value) {
    ob_check_space(b);
    SET_VECTOR_ELT(b->options, b->cnt, value);
    SET_STRING_ELT(b->names, b->cnt, mkChar(name));
    b->cnt++;
}

/* Add a string value (from const char*). mkString is safe without extra PROTECT
   because options is already PROTECTed and SET_VECTOR_ELT stores the element. */
void ob_add_string(OptionBuilder *b, const char *name, const char *value) {
    ob_check_space(b);
    SET_VECTOR_ELT(b->options, b->cnt, mkString(value));
    SET_STRING_ELT(b->names, b->cnt, mkChar(name));
    b->cnt++;
}

/* Add an integer scalar. Uses ScalarInteger. */
void ob_add_int(OptionBuilder *b, const char *name, int value) {
    ob_check_space(b);
    SET_VECTOR_ELT(b->options, b->cnt, ScalarInteger(value));
    SET_STRING_ELT(b->names, b->cnt, mkChar(name));
    b->cnt++;
}

/* Add a real (double) scalar. Uses ScalarReal. */
void ob_add_real(OptionBuilder *b, const char *name, double value) {
    ob_check_space(b);
    SET_VECTOR_ELT(b->options, b->cnt, ScalarReal(value));
    SET_STRING_ELT(b->names, b->cnt, mkChar(name));
    b->cnt++;
}

/* Logical scalar (value should be 0, 1 or NA_LOGICAL) */
void ob_add_logical(OptionBuilder *b, const char *name, int value) {
    ob_check_space(b);
    SET_VECTOR_ELT(b->options, b->cnt, ScalarLogical(value)); 
    SET_STRING_ELT(b->names, b->cnt, mkChar(name)); 
    b->cnt++;
}

/* Finalize: attach names, unprotect the two protected objects and return options.
   After this call the builder should not be used again. */
SEXP ob_get(OptionBuilder *b) {
    if (b->cnt != b->n) {
        Rf_error("OptionBuilder: expected %d options but only %d were added",
                 b->n, b->cnt);
    }
    setAttrib(b->options, R_NamesSymbol, b->names);
    UNPROTECT(2); /* options & names */
    return b->options;
}

