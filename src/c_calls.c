#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include "report.h"
#include "get_info.h"
#include "c_calls.h"

#define MAX_NAME_LEN 32

extern void F77_NAME(read_model_fortran)(int *modelnmlen, const char *modelnm,
                                       int *model_index, int *ier);
extern void F77_NAME(get_data_fortran)(int *mws_index, int *nvar, int *ivar, 
                                       int *ntime, int *jtb,
                                       int *jte, double *data);
extern void F77_NAME(get_ca_fortran)(int *mws_index, int *nca, int *ica, 
                                     int *ntime, int *jtb,
                                     int *jte, double *data);
extern void F77_NAME(set_data_fortran)(int *mws_index, int *nvar, int *ivar, 
                                       int *ntime, int *jtb, int *jte, 
                                       double *data, int *icol);
extern void F77_NAME(set_ca_fortran)(int *mws_index, int *, int *ivar, 
                                       int *ntime, int *jtb, int *jte, 
                                       double *data, int *icol);
extern void F77_NAME(set_fix_fit_fortran)(int *mws_index, int *, int *ivar, 
                                      int *ntime, int *jtb, int *jte, 
                                      double *data, int *icol, int *fix);
extern void F77_NAME(solve_fortran)(int *mws_index, int *startp, int *endp,
                                    int *error);
extern void F77_NAME(filmdt_fortran)(int *mws_index, int *startp, int *endp);
extern void F77_NAME(set_rms_fortran)(int *mws_index, int *var_index,
                                      double *value);

SEXP read_mdl_c(SEXP filename) {

    const char *modelnm = CHAR(STRING_ELT(filename, 0));
    int modelnmlen = strlen(modelnm);
    int model_index, ier;
    F77_CALL(read_model_fortran)(&modelnmlen, modelnm, &model_index, &ier);

    if (ier == 0) {
        return ScalarInteger(model_index);
    } else {
        switch (ier) {
        case 1:  
            error("Cannot open Mif file %s\n", modelnm);
            break;
        // TODO: handle other errors
        default:
            error("Unknown error reading Mif file %s\n", modelnm);
        }
        return ScalarInteger(-1);
    }
}

/* general function for obtaining a character vector with special
 * model variables */
SEXP get_names_c(int model_index, int nvar,
                 void  (*get_name)(int *, int *, char *, int *)) {
    char name[MAX_NAME_LEN + 1];  // 1 extra character for terminating 0.
    int ivar, len;
    SEXP names = PROTECT(allocVector(STRSXP, nvar));
    for (ivar = 1; ivar <= nvar; ivar++) {
        (*get_name)(&model_index, &ivar, name, &len);
        name[len] = '\0';
        SET_STRING_ELT(names, ivar - 1, mkChar(name));
    }
    UNPROTECT(1);
    return names;
}

/* return the name of ivar's model variable in alphabetical ordering */
void get_variable_name_alpha(int *model_index, int *ivar, char *name, 
                             int *len) {
    int alphabet = 1;
    F77_CALL(get_variable_name)(model_index, ivar, name, len, &alphabet);
}

SEXP get_variable_names_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int nvar = F77_CALL(get_variable_count)(&model_index);
    return get_names_c(model_index, nvar, get_variable_name_alpha);
}

SEXP get_ca_names_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int nvar = F77_CALL(get_ca_count)(&model_index);
    return get_names_c(model_index, nvar, F77_CALL(get_ca_name));
}

/* General function for getting model data or constant adjustments */
SEXP get_data_or_ca(SEXP mws_index_, SEXP names, SEXP jtb_, SEXP jte_,
                      int (*get_index)(int *, const char *, int *),
                      void (*get_mat)(int *, int *, int *, int *, int *, int *,
                                      double *)) {

    int mws_index = asInteger(mws_index_);
    int jtb = asInteger(jtb_);
    int jte = asInteger(jte_);
    int ntime = jte - jtb + 1;
    int nvar = length(names);
    SEXP data = PROTECT(allocVector(REALSXP, ntime * nvar));

    /* convert into matrix */
    SEXP dim = PROTECT(allocVector(INTSXP, 2));
    INTEGER(dim)[0] = ntime;
    INTEGER(dim)[1] = nvar;
    setAttrib(data, R_DimSymbol, dim);
    
    // fill in model data
    int *ivar = (int *) R_alloc(nvar, sizeof(int));
    for (int i = 0; i < nvar; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        ivar[i] = (*get_index)(&mws_index, name, &namelen);
    }

    (*get_mat)(&mws_index, &nvar, ivar, &ntime, &jtb, &jte, REAL(data));

    UNPROTECT(2);
    
    return data;
}


/* Returns a matrix with mdl data values. names is a character vector
 * with the names of the variables. This vector should only contain names
 * of existing model variables. Thus make sure to remove any name that
 * is not present in the model data
 * before calling this function */
SEXP get_data_c(SEXP mws_index_, SEXP names, SEXP jtb_, SEXP jte_) {
    return get_data_or_ca(mws_index_, names, jtb_, jte_, 
                   F77_CALL(get_var_index), F77_CALL(get_data_fortran));
}

/* Returns a matrix with CA values. names is a character vector
 * with the names of the CAs. This vector should only contain names
 * of exsting CA CA alues! Thus make sure to remove any CA names that
 * before calling this function */
SEXP get_ca_c(SEXP mws_index_, SEXP names, SEXP jtb_, SEXP jte_) {
    return get_data_or_ca(mws_index_, names, jtb_, jte_, 
                   F77_CALL(get_ca_index), F77_CALL(get_ca_fortran));
}

SEXP set_c(SEXP set_type_, SEXP mws_index_, SEXP mat, SEXP shift_) {
    int set_type = asInteger(set_type_);
    int mws_index = asInteger(mws_index_);
    int shift = asInteger(shift_);

    SEXP dim = getAttrib(mat, R_DimSymbol);
    int ntime = INTEGER(dim)[0];

    SEXP dim_names = getAttrib(mat, R_DimNamesSymbol);
    SEXP col_names = VECTOR_ELT(dim_names, 1);
    int n_names = length(col_names);

    int *ivar = (int *) R_alloc(n_names, sizeof(int));
    int *icol = (int *) R_alloc(n_names, sizeof(int));
    int ic;
    int nvar = 0;
    for (ic = 0; ic < n_names; ic++) {
        const char *name = CHAR(STRING_ELT(col_names, ic));
        int namelen = strlen(name);
        int iv = F77_CALL(get_var_index)(&mws_index, name, &namelen);
        if (iv > 0) {
            /* add 1 because 1 based indexing in Fortr. */
            icol[nvar]   = ic + 1; 
            ivar[nvar++] = iv;
        }
    }

    int jtb = shift;
    int jte = shift + ntime - 1;

    int fix;

    switch (set_type) {
        case SET_DATA:
            F77_CALL(set_data_fortran)(&mws_index, &nvar, ivar, &ntime, &jtb,
                                       &jte, REAL(mat), icol);
            break;
        case SET_CA:
            F77_CALL(set_ca_fortran)(&mws_index, &nvar, ivar, &ntime, &jtb,
                                     &jte, REAL(mat), icol);
            break;
        case SET_FIX:
        case SET_FIT:
            fix = set_type == SET_FIX;
            F77_CALL(set_fix_fit_fortran)(&mws_index, &nvar, ivar, &ntime, &jtb, 
                                          &jte, REAL(mat), icol, &fix);
            break;
    }
    return R_NilValue;
}

SEXP set_rms_c(SEXP mws_index_, SEXP rms_list) {
    int mws_index = asInteger(mws_index_);
    SEXP names = getAttrib(rms_list, R_NamesSymbol);
    int n_names = length(names);
    int i;
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int iv = F77_CALL(get_var_index)(&mws_index, name, &namelen);
        SEXP d = VECTOR_ELT(rms_list, i);
        double value = asReal(d);
        // todo: error if value if not a numerical vector.
        // or maybe it is easier to do this test in pure fortran
        F77_NAME(set_rms_fortran)(&mws_index, &iv, &value);
    }
    return R_NilValue;
}

SEXP solve_c(SEXP mws_index_, SEXP startp_, SEXP endp_, SEXP period_string_) {

    // process arguments
    int mws_index = asInteger(mws_index_);
    int startp = asInteger(startp_);
    int endp= asInteger(endp_);
    const char *period_string = CHAR(STRING_ELT(period_string_, 0));

    init_solve_report(period_string);
    int error;
    F77_CALL(solve_fortran)(&mws_index, &startp, &endp, &error);
    char *report_text = close_report();
    char *summary = get_summary();
    if (error) {
        warning(
"\nSolve model issued error messages.\nThe solve may be unsuccesfull/incomplete or erroneous.\nSee the solve_report returned by solve for details.");
    }

    // create ouput: solve_report
    SEXP list = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(list, 0, ScalarLogical(!error));
    SET_VECTOR_ELT(list, 1, mkString(summary));
    SET_VECTOR_ELT(list, 2, mkString(report_text));
    SEXP names = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("success"));
    SET_STRING_ELT(names, 1, mkChar("message"));
    SET_STRING_ELT(names, 2, mkChar("full_report"));
    setAttrib(list, R_NamesSymbol, names);
    UNPROTECT(2);
    return list;
}


SEXP filmdt_c(SEXP mws_index_, SEXP startp_, SEXP endp_, SEXP period_string_) {

    // process arguments
    int mws_index = asInteger(mws_index_);
    int startp = asInteger(startp_);
    int endp= asInteger(endp_);
    const char *period_string = CHAR(STRING_ELT(period_string_, 0));

    init_filmdt_report(period_string);
    F77_CALL(filmdt_fortran)(&mws_index, &startp, &endp);
    char *report_text = close_report();

    // create ouput: solve_report
    SEXP ret = mkString(report_text);
    return ret;
}


