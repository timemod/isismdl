#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include "get_info.h"
#include "c_calls.h"
#include "process_solve_options.h"

#define MAX_NAME_LEN 32
#define DATA  1
#define CA    2
#define FIX   3
#define FIT   4

extern void F77_NAME(read_model_fortran)(int *modelnmlen, const char *modelnm,
                                       int *model_index, int *ier);
extern void F77_NAME(get_data_fortran)(int *mws_index, int *nvar, int *ivar, 
                                       int *ntime, int *jtb,
                                       int *jte, double *data);
extern void F77_NAME(get_ca_fortran)(int *mws_index, int *nca, int *ica, 
                                     int *ntime, int *jtb,
                                     int *jte, double *data);
extern void F77_NAME(get_fix_fit_fortran)(int *mws_index, int *nvar, int *ivar, 
                                          int *ntime, int *jtb, int *jte, 
                                          double *mat, int *fix_);
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
                                    int *opts_present, int *error);
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

SEXP get_names_c(SEXP type_, SEXP model_index_) {
    const char *type_str = CHAR(STRING_ELT(type_, 0));
    int type;
    if (strcmp(type_str, "ca") == 0) {
        type = CA;
    } else {
        type = DATA;
    }
    int model_index = asInteger(model_index_);

    int nvar;
    if (type == CA) {
        nvar = F77_CALL(get_ca_count)(&model_index);
    } else {
        nvar = F77_CALL(get_variable_count)(&model_index);
    }

    void (*get_name)(int *, int *, char *, int *);
    if (type == CA) {
        get_name = F77_CALL(get_ca_name);
    } else {
        get_name = F77_CALL(get_variable_name);
    }
    
    /* get list of variables */
    int ivar, len;
    char name[MAX_NAME_LEN + 1]; /* +1 because of terminating '\0' */
    SEXP names = PROTECT(allocVector(STRSXP, nvar));
    for (ivar = 1; ivar <= nvar; ivar++) {
        (*get_name)(&model_index, &ivar, name, &len);
        name[len] = '\0';
        SET_STRING_ELT(names, ivar - 1, mkChar(name));
    }
    UNPROTECT(1);
    return names;
}

/* General function for getting model data or constant adjustments */
SEXP get_data_c(SEXP type_, SEXP mws_index_, SEXP names, SEXP jtb_, SEXP jte_) {

    const char *type_str = CHAR(STRING_ELT(type_, 0));
    int type;
    if (strcmp(type_str, "ca") == 0) {
         type = CA;
    } else {
        type = DATA;
    }

    int mws_index = asInteger(mws_index_);
    int jtb = asInteger(jtb_);
    int jte = asInteger(jte_);
    int ntime = jte - jtb + 1;
    int nvar = length(names);

    /* set the function used to convert the names to an indeces */
    int (*get_index)(int *, const char *, int *);
    if (type == CA) {
        get_index = F77_CALL(get_ca_index);
    } else {
        get_index = F77_CALL(get_var_index);
    }
    
    /* get list of variables */
    int *ivar = (int *) R_alloc(nvar, sizeof(int));
    for (int i = 0; i < nvar; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        ivar[i] = (*get_index)(&mws_index, name, &namelen);
    }

    /* set the function used to get the data */
    void (*get_dat)(int *, int *, int *, int *, int *, int *, double *);
    if (type == CA) {
        get_dat = F77_CALL(get_ca_fortran);
    } else {
        get_dat = F77_CALL(get_data_fortran);
    }

    /* get the data */
    SEXP data = PROTECT(allocVector(REALSXP, ntime * nvar));
    (*get_dat)(&mws_index, &nvar, ivar, &ntime, &jtb, &jte, REAL(data));
    
    /* set the dimension of the matrix */
    SEXP dim = PROTECT(allocVector(INTSXP, 2));
    INTEGER(dim)[0] = ntime;
    INTEGER(dim)[1] = nvar;
    setAttrib(data, R_DimSymbol, dim);


    UNPROTECT(2);
    return data;
}


void set_c(SEXP set_type_, SEXP mws_index_, SEXP mat, SEXP shift_) {
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
}

/* General function for getting fix value or fit values */
SEXP get_fix_fit_c(SEXP type_, SEXP mws_index_) {
    const char *type_str = CHAR(STRING_ELT(type_, 0));
    int fix = strcmp(type_str, "fix") == 0 ? 1 : 0;
    int mws_index = asInteger(mws_index_);
    int nvar, jtb, jte;

    if (fix) {
        F77_CALL(get_fix_info)(&mws_index, &nvar, &jtb, &jte);
    } else  {
        F77_CALL(get_fit_info)(&mws_index, &nvar, &jtb, &jte);
    }

    if (nvar == 0) {
        return R_NilValue;
    }

    int ntime = jte - jtb + 1;
    SEXP mat = PROTECT(allocVector(REALSXP, ntime * nvar));
    int *ivar = (int *) R_alloc(nvar, sizeof(int));
    F77_CALL(get_fix_fit_fortran)(&mws_index, &nvar, ivar, &ntime, &jtb, 
                                  &jte, REAL(mat), &fix);

    /* set the dimension of the matrix */
    SEXP dim = PROTECT(allocVector(INTSXP, 2));
    INTEGER(dim)[0] = ntime;
    INTEGER(dim)[1] = nvar;
    setAttrib(mat, R_DimSymbol, dim);

    char name[MAX_NAME_LEN + 1]; /* +1 because of terminating '\0' */
    SEXP names = PROTECT(allocVector(STRSXP, nvar));
    int i, len;
    for (i = 0; i < nvar; i++) {
        F77_CALL(get_variable_name)(&mws_index, &ivar[i], name, &len);
        name[len] = '\0';
        SET_STRING_ELT(names, i, mkChar(name));
    }

    // create ouput list
    SEXP list = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(list, 0, ScalarInteger(jtb));
    SET_VECTOR_ELT(list, 1, mat);
    SET_VECTOR_ELT(list, 2, names);

    UNPROTECT(4);
    return list;
}

void set_rms_c(SEXP mws_index_, SEXP rms_list) {
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
}

void solve_c(SEXP mws_index_, SEXP startp_, SEXP endp_, SEXP options) {

    // process arguments
    int mws_index = asInteger(mws_index_);
    int startp = asInteger(startp_);
    int endp= asInteger(endp_);

    int opts_present = length(options) > 0;
    if (opts_present) {
        process_solve_options(&mws_index, options);
    }
    int error;
    F77_CALL(solve_fortran)(&mws_index, &startp, &endp, &opts_present, &error);
}


void filmdt_c(SEXP mws_index_, SEXP startp_, SEXP endp_) {
    int mws_index = asInteger(mws_index_);
    int startp = asInteger(startp_);
    int endp= asInteger(endp_);
    F77_CALL(filmdt_fortran)(&mws_index, &startp, &endp);
}


