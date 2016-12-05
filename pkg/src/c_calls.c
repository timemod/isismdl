#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include "get_info.h"
#include "c_calls.h"
#include "set_solve_options.h"
#include "set_fit_options.h"

#define MAX_NAME_LEN 32
#define ALL     1
#define ALLFRML 2
#define ALL_ENDOLEAD 3

#define DATA    1
#define CA     2
#define FIX   3
#define FIT   4

#define ALL_EQ      1
#define INACTIVE_EQ 2

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
extern void F77_NAME(get_param_fortran)(int *mws_index, int *ip, double *value,
                                        int *par_len);
extern int F77_NAME(set_param_fortran)(int *mws_index, int *ipar, double *data,
                                        int *len);
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
extern void F77_NAME(set_test)(int *mws_index, int *var_index, double *value);
extern double F77_NAME(get_test)(int *mws_index, int *var_index, int *alphabet);
extern void F77_NAME(activate_equation)(int *mws_index, int *var_index);
extern void F77_NAME(deactivate_equation)(int *mws_index, int *var_index);
extern void F77_NAME(set_ftrelax)(int *mws_index, int *var_index, double *value);
extern double F77_NAME(get_ftrelax)(int *mws_index, int *var_index);

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

/* Returns the names of the model parameters */
SEXP get_par_names_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int npar = F77_CALL(get_param_count)(&model_index);

    /* get list of parameters */
    int ipar, len, alpha = 1;
    char name[MAX_NAME_LEN + 1]; /* +1 because of terminating '\0' */
    SEXP names = PROTECT(allocVector(STRSXP, npar));
    for (ipar = 1; ipar <= npar; ipar++) {
        F77_CALL(get_param_name)(&model_index, &ipar, name, &len, &alpha);
        name[len] = '\0';
        SET_STRING_ELT(names, ipar - 1, mkChar(name));
    }
    UNPROTECT(1);
    return names;
}

/* Returns variable names */
SEXP get_var_names_c(SEXP type_, SEXP model_index_) {
    const char *type_str = CHAR(asChar(type_));
    int type;
    if (strcmp(type_str, "all") == 0) {
        type = ALL;
    } else if (strcmp(type_str, "allfrml") == 0) {
        type = ALLFRML;
    } else  if (strcmp(type_str, "all_endolead") == 0) {
        type = ALL_ENDOLEAD;
    } else {
        error("Illegal parameter vtype %s\n", type_str);
    } 
    int model_index = asInteger(model_index_);
    int alpha = 1;

    int nvar;
    void (*get_name)(int *, int *, char *, int *);
    switch (type) {
    case ALL:          nvar = F77_CALL(get_variable_count)(&model_index);
                       break;
    case ALLFRML:      nvar = F77_CALL(get_ca_count)(&model_index); 
                       get_name = F77_CALL(get_ca_name);
                       break;
    case ALL_ENDOLEAD: nvar = F77_CALL(get_endex_count)(&model_index); 
                       get_name = F77_CALL(get_endex_name);
                       break;
    }

    /* get list of variables */
    int ivar, len;
    char name[MAX_NAME_LEN + 1]; /* +1 because of terminating '\0' */
    SEXP names = PROTECT(allocVector(STRSXP, nvar));
    for (ivar = 1; ivar <= nvar; ivar++) {
        if (type == ALL) {
            F77_CALL(get_variable_name)(&model_index, &ivar, name, &len,
                                        &alpha);
        } else {
            (*get_name)(&model_index, &ivar, name, &len);
        }
        name[len] = '\0';
        SET_STRING_ELT(names, ivar - 1, mkChar(name));
    }
    UNPROTECT(1);
    return names;
}

/* get equation names sorted alphabetically */
SEXP get_eq_names_c(SEXP type_, SEXP model_index_) {
    int model_index = asInteger(model_index_);
    const char *type_str = CHAR(asChar(type_));
    int type;
    if (strcmp(type_str, "all") == 0) {
        type = ALL_EQ;
    } else if (strcmp(type_str, "inactive") == 0) {
        type = INACTIVE_EQ;
    } else {
        error("Illegal equation type %s specified\n", type_str);
    }

    int neq  = F77_CALL(get_eq_count)(&model_index);
    int alphabet = 1;

    /* get list of equation indices */
    int *ieq = (int *) R_alloc(neq, sizeof(int));
    int cnt = 0;
    int i, ok;
    for (i = 1; i <= neq; i++) {
        if (type == INACTIVE_EQ) {
            ok = ! F77_CALL(equation_is_active)(&model_index, &i, &alphabet);
        } else {
            ok = 1;
        }
        if (ok) {
            ieq[cnt++] = i;
        }
    }

    /* now create chacracter vector with the selected equation names */
    int len;
    char name[MAX_NAME_LEN + 1]; /* +1 because of terminating '\0' */
    SEXP names = PROTECT(allocVector(STRSXP, cnt));
    for (i = 0; i < cnt; i++) {
        F77_CALL(get_equation_name)(&model_index, ieq + i, name, &len,
                                    &alphabet);
        name[len] = '\0';
        SET_STRING_ELT(names, i, mkChar(name));
    }
    UNPROTECT(1);
    return names;
}

/* Get the model parameters */
SEXP get_param_c(SEXP mws_index_, SEXP names) {
    int mws_index = asInteger(mws_index_);
    int npar = length(names);
    int *ipar = (int *) R_alloc(npar, sizeof(int));
    int cnt = 0;
    for (int i = 0; i < npar; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int ip = F77_CALL(get_par_index)(&mws_index, name, &namelen);
        if (ip > 0) {
            ipar[cnt++] = ip;
        } else {
           warning("\"%s\" is not a parameter\n", name);
        }
    }
    if (cnt < npar) {
        error("%d incorrect parameter name(s) encountered. See warning(s).\n", 
              npar - cnt);
    }
    SEXP retval = PROTECT(allocVector(VECSXP, cnt));
    for (int i = 0; i < cnt; i++) {
        int par_len = F77_CALL(get_param_length)(&mws_index, ipar + i);
        SEXP value = PROTECT(allocVector(REALSXP,  par_len));
        F77_CALL(get_param_fortran)(&mws_index, ipar + i, REAL(value), &par_len);
        SET_VECTOR_ELT(retval,  i, value);
    }

    setAttrib(retval, R_NamesSymbol, names);
    UNPROTECT(npar + 1);
    return retval;
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

    /* set the function used to convert the names to a indices */
    int (*get_index)(int *, const char *, int *);
    const char *desc;
    if (type == CA) {
        get_index = F77_CALL(get_ca_index);
        desc = "constant adjustment";
    } else {
        get_index = F77_CALL(get_var_index);
        desc = "model variable";
    }
    
    /* get list of variables */
    int cnt = 0;
    int *ivar = (int *) R_alloc(nvar, sizeof(int));
    for (int i = 0; i < nvar; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int idx = (*get_index)(&mws_index, name, &namelen);
        if (idx > 0) {
            ivar[cnt++] = (*get_index)(&mws_index, name, &namelen);
        } else {
            warning("\"%s\" is not a %s\n", name, desc);
        }
    }

    if (cnt < nvar) {
        error("%d incorrect variable name(s) encountered. See warning(s).\n", 
              nvar - cnt);
    }

    /* set the function used to get the data */
    void (*get_dat)(int *, int *, int *, int *, int *, int *, double *);
    if (type == CA) {
        get_dat = F77_CALL(get_ca_fortran);
    } else {
        get_dat = F77_CALL(get_data_fortran);
    }

    /* get the data */
    SEXP data = PROTECT(allocVector(REALSXP, ntime * cnt));
    (*get_dat)(&mws_index, &cnt, ivar, &ntime, &jtb, &jte, REAL(data));
    
    /* set the dimension of the matrix */
    SEXP dim = PROTECT(allocVector(INTSXP, 2));
    INTEGER(dim)[0] = ntime;
    INTEGER(dim)[1] = cnt;
    setAttrib(data, R_DimSymbol, dim);


    UNPROTECT(2);
    return data;
}

/* Sets the model parameters. Returns the number of parameters actually set */
SEXP set_param_c(SEXP mws_index_, SEXP param_list) {
    int mws_index = asInteger(mws_index_);
    SEXP names = getAttrib(param_list, R_NamesSymbol);
    int n_names = length(names);
    int i;
    int cnt = 0;
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int ip = F77_CALL(get_par_index)(&mws_index, name, &namelen);
        if (ip > 0) {   
            cnt++;
            SEXP value = VECTOR_ELT(param_list, i);
            int len = length(value);
            int ret = F77_CALL(set_param_fortran)(&mws_index, &ip, REAL(value), 
                               &len);
            if (ret == 1) {
                error("Value for parameter %s has an incorrect length. Required length: %d. Actual length: %d", name, F77_CALL(get_param_length)(&mws_index, &ip), len);
            }
        }
    }
    return ScalarInteger(cnt);
}

void set_c(SEXP set_type_, SEXP mws_index_, SEXP mat, SEXP names, SEXP shift_) {
    int set_type = asInteger(set_type_);
    int mws_index = asInteger(mws_index_);
    int shift = asInteger(shift_);

    SEXP dim = getAttrib(mat, R_DimSymbol);
    int ntime = INTEGER(dim)[0];

    int n_names = length(names);

    int *ivar = (int *) R_alloc(n_names, sizeof(int));
    int *icol = (int *) R_alloc(n_names, sizeof(int));
    int ic;
    int nvar = 0;
    for (ic = 0; ic < n_names; ic++) {
        const char *name = CHAR(STRING_ELT(names, ic));
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
    int i, len, alpha = 0;
    for (i = 0; i < nvar; i++) {
        F77_CALL(get_variable_name)(&mws_index, &ivar[i], name, &len, &alpha);
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

void set_rms_c(SEXP mws_index_, SEXP values) {
    int mws_index = asInteger(mws_index_);
    SEXP names = getAttrib(values, R_NamesSymbol);
    int n_names = length(values);
    int i;
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int iv = F77_CALL(get_var_index)(&mws_index, name, &namelen);
        double value = REAL(values)[i];
        F77_NAME(set_rms_fortran)(&mws_index, &iv, &value);
    }
}

void solve_c(SEXP mws_index_, SEXP startp_, SEXP endp_, SEXP options,
             SEXP fit_options) {

    // process arguments
    int mws_index = asInteger(mws_index_);
    int startp = asInteger(startp_);
    int endp= asInteger(endp_);

    int opts_present = length(options) > 0;
    if (opts_present) {
        int use_mws = 0;
        set_solve_options(&mws_index, &use_mws, options);
    }
    int fit_opts_present = length(fit_options) > 0;
    if (fit_opts_present) {
        int use_mws = 0;
        set_fit_options(&mws_index, &use_mws, fit_options);
        opts_present = 1;
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

/* Sets convergence criterium for some model variables */
void set_cvgcrit_c(SEXP mws_index_, SEXP names, SEXP value_) {
    int mws_index = asInteger(mws_index_);
    double value = asReal(value_);
    int i, cnt = 0;
    int n_names =  length(names);
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int iv = F77_CALL(get_var_index)(&mws_index, name, &namelen);
        if (iv > 0) {   
            cnt++;
            F77_CALL(set_test)(&mws_index, &iv, &value);
        } else {
           warning("\"%s\" is not a model variable\n", name);
        }
    }

    if (cnt < n_names) {
        error("%d incorrect variable name(s) encountered. See warning(s).\n", 
              n_names - cnt);
    }
}

/* Sets convergence criterium for all model variables, used in set_mws.
 * values is a vector with convergence criteria for the model variables
 * in natural (i.e. non-alphabetical) order */
void set_cvgcrit_set_mws(SEXP mws_index_, SEXP values) {
    int mws_index = asInteger(mws_index_);
    int i;
    for (i = 1; i <- length(values); i++) {
        double value = REAL(values)[i - 1];
        F77_CALL(set_test)(&mws_index, &i, &value);
    }
}

/* Returns the convergence criteria for all model variables */
SEXP get_cvgcrit_c(SEXP mws_index_, SEXP alphabet_) {
    int mws_index = asInteger(mws_index_);
    int alphabet = asInteger(alphabet_);
    int nvar = F77_CALL(get_variable_count)(&mws_index);
    SEXP ret = PROTECT(allocVector(REALSXP, nvar));
    int iv;
    for (iv = 1; iv <= nvar; iv++) {
        REAL(ret)[iv - 1] = F77_CALL(get_test)(&mws_index, &iv, &alphabet);
    }
    UNPROTECT(1);
    return ret;
}

/* Sets Fair-Taylor relaxtion factors */
void set_ftrelax_c(SEXP mws_index_, SEXP names, SEXP value_) {
    int mws_index = asInteger(mws_index_);
    double value = asReal(value_);
    int i, cnt = 0;
    int n_names =  length(names);
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int iendex = F77_CALL(get_iendex)(&mws_index, name, &namelen);
        if (iendex > 0) {   
            cnt++;
            F77_CALL(set_ftrelax)(&mws_index, &iendex, &value);
        } else {
           warning("\"%s\" is not an endogenous lead.\n", name);
        }
    }

    if (cnt < n_names) {
        error("%d incorrect variable name(s) encountered. See warning(s).\n", 
              n_names - cnt);
    }
}

/* Sets Fair-Taylor relaxation factors for all endogenous leads.
 * in natural (i.e. non-alphabetical) order */
void set_ftrelax_set_mws(SEXP mws_index_, SEXP values) {
    int mws_index = asInteger(mws_index_);
    int i;
    for (i = 1; i <= length(values); i++) {
        double value = REAL(values)[i - 1];
        F77_CALL(set_ftrelax)(&mws_index, &i, &value);
    }
}

/* Returns the convergence criteria for all model variables */

/* Returns the Fair-Taylor relaxation factors for all endogenous
 * leads (not in alphabetical order). */
SEXP get_ftrelax_c(SEXP mws_index_) {
    int mws_index = asInteger(mws_index_);
    int nendex = F77_CALL(get_endex_count)(&mws_index);
    SEXP ret = PROTECT(allocVector(REALSXP, nendex));
    int iendex;
    for (iendex = 1; iendex <= nendex; iendex++) {
        REAL(ret)[iendex - 1] = F77_CALL(get_ftrelax)(&mws_index, &iendex);
    }
    UNPROTECT(1);
    return ret;
}


/* Activate/deactive equation */
void set_eq_status_c(SEXP mws_index_, SEXP names, SEXP status) {
    int mws_index = asInteger(mws_index_);
    const char *status_str = CHAR(asChar(status));
    
    int activate;
    if (strcmp(status_str, "active") == 0) {
        activate = 1;
    } else if (strcmp(status_str, "inactive") == 0) {
        activate = 0;
    } else {
        error("Illegal option %s for argument status\n", status_str);
    }

    void (*fun)(int *, int *);
    if (activate) {
        fun = F77_CALL(activate_equation);
    } else {
        fun = F77_CALL(deactivate_equation);
    }

    int n_names = length(names);
    int i;
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int ieq = F77_CALL(get_eq_index)(&mws_index, name, &namelen);
        if (ieq > 0) {
            fun(&mws_index, &ieq);
        }
    }
}
