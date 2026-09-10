#include <Rinternals.h>
#include <Rdefines.h>
#include <string.h>
#include "get_info.h"
#include "fortran_calls.h"
#include "c_calls.h"
#include "set_solve_options.h"
#include "set_fit_options.h"
#include "option_utils.h"
#include "init_set_get_options.h"
#include "isismdl_types.h"
#include "get_solve_jac_info.h"

#define ALL  1
#define FRML 2
#define ENDOLEADS 3
#define LAGS 4
#define LEADS 5
#define FEEDBACK 6

#define DATA    1
#define CA     2
#define FIX   3
#define FIT   4

#define ALL      1
#define ACTIVE   2
#define INACTIVE 3

extern void F77_NAME(init_modules_fortran)(void);
extern void F77_NAME(read_model_fortran)(int *mifnmlen, const char *mifnm,
                                         int *model_index, int *reorder_names, 
                                         int *ier);
extern void F77_NAME(write_model_fortran)(int *mifnmlen, const char *mifnm,
                                          int *model_index, int *ier);


/* Return an unprotected SEXP with variable names from an integer array of variable 
 * indices.*/
static inline SEXP get_variable_names(int *indices, int n, int model_index) {

    SEXP names = PROTECT(allocVector(STRSXP, n));

    char name[MCMXNM + 1]; /* +1 because of terminating '\0' */
    int alpha = 0;
    int i, len, index;

    for (i = 0; i < n; i++) {
        index = indices[i];
        F77_CALL(get_variable_name)(&model_index, &index, name, &len, &alpha);
        name[len] = '\0';
        SET_STRING_ELT(names, i, mkChar(name));
    }

    UNPROTECT(1);

    return names;
}

static inline SEXP get_lags_or_leads(int model_index, int type) {

    /* returns a character vector with the names of lags or leads */

    int nv = F77_CALL(get_variable_count)(&model_index);
    int *ivars = (int *) R_alloc(nv, sizeof(int));

    int (*has_lag_or_lead)(int *, int *);
    if (type == LAGS) {
        has_lag_or_lead = F77_CALL(has_lag);
    } else {
        has_lag_or_lead = F77_CALL(has_lead);
    }

    // get indices of variables
    int iv, ok, cnt = 0;
    for (iv = 1; iv <= nv; iv++) {
      ok = (*has_lag_or_lead)(&model_index, &iv);
      if (ok) {
          ivars[cnt++] = iv;
      }
    }

    return get_variable_names(ivars, cnt, model_index);
}


/* Returns an unprotected SEXP that holds a numerical matrix */
static inline SEXP create_matrix(int nrows, int ncols) {

   SEXP mat = PROTECT(allocVector(REALSXP, nrows * ncols));

   SEXP dim = PROTECT(allocVector(INTSXP, 2));
   INTEGER(dim)[0] = nrows;
   INTEGER(dim)[1] = ncols;
   setAttrib(mat, R_DimSymbol, dim);
 
   UNPROTECT(2);

   return mat;
}

/* Add row and column names to an SEXP that holds matrix */
static inline SEXP add_row_and_column_names(SEXP mat, SEXP row_names,
                                            SEXP col_names) {

    SEXP dnames = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dnames, 0, row_names);
    SET_VECTOR_ELT(dnames, 1, col_names);
    setAttrib(mat, R_DimNamesSymbol, dnames);

    UNPROTECT(1);

    return mat;
}

SEXP init_modules_c(void) {
    F77_CALL(init_modules_fortran)();	
    return R_NilValue;
}

SEXP read_mdl_c(SEXP filename, SEXP reorder_names_) {

    const char *mifnm = CHAR(STRING_ELT(filename, 0));
    int mifnmlen = strlen(mifnm);
    int model_index, ier;
    int reorder_names = asInteger(reorder_names_);
    F77_CALL(read_model_fortran)(&mifnmlen, mifnm, &model_index, 
                                 &reorder_names, &ier);

    if (ier == 0) {
        return ScalarInteger(model_index);
    } else {
        switch (ier) {
        case 1:
            error("Cannot open Mif file %s", mifnm);
            break;
        case 3:
            error("Model not correct (see output)");
            break;
        case 4:
            error("Out of memory");
            break;
        default:
            error("Unknown error reading Mif file %s", mifnm);
            break;
        }
        return ScalarInteger(-1);
    }
}

/* Write the model to a mif file,
 * used in methods write_mdl() and serialize(). */
SEXP write_mdl_c(SEXP filename, SEXP model_index_) {

    int model_index = asInteger(model_index_);
    const char *mifnm = CHAR(STRING_ELT(filename, 0));
    int mifnmlen = strlen(mifnm);
    int ier;

    F77_CALL(write_model_fortran)(&mifnmlen, mifnm, &model_index, &ier);

    if (ier != 0) {
        if (ier == 5) {
            error("Cannot open Mif file %s for writing\n", mifnm);
        } else if (ier == 6) {
             error("Error writing MIF file %s\n", mifnm);
        } else {
             error("Unknown error %d writing Mif file %s\n", ier, mifnm);
        }
    }
    return R_NilValue;
}

/* Returns the names of the model parameters */
SEXP get_par_names_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int npar = F77_CALL(get_param_count)(&model_index);

    /* get list of parameters */
    int ipar, len, alpha = 1;
    char name[MCMXNM + 1]; /* +1 because of terminating '\0' */
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
    } else if (strcmp(type_str, "frml") == 0) {
        type = FRML;
    } else  if (strcmp(type_str, "endolead") == 0) {
        type = ENDOLEADS;
    } else  if (strcmp(type_str, "lags") == 0) {
        type = LAGS;
    } else  if (strcmp(type_str, "leads") == 0) {
        type = LEADS;
    } else  if (strcmp(type_str, "feedback") == 0) {
        type = FEEDBACK;
    } else {
        error("Illegal parameter vtype %s\n", type_str);
    }
    int model_index = asInteger(model_index_);

    if (type == LAGS || type == LEADS) {
        return get_lags_or_leads(model_index, type);
    }

    int alpha = 1;

    int nvar;
    void (*get_name)(int *, int *, char *, int *);
    switch (type) {
    case ALL:      nvar = F77_CALL(get_variable_count)(&model_index);
                   get_name = NULL;
                   break;
    case FRML:     nvar = F77_CALL(get_ca_count)(&model_index);
                   get_name = F77_CALL(get_ca_name);
                   break;
    case ENDOLEADS: nvar = F77_CALL(get_endex_count)(&model_index);
                   get_name = F77_CALL(get_endex_name);
                   break;
    case FEEDBACK: nvar = F77_CALL(get_fb_count)(&model_index);
                   get_name = F77_CALL(get_fb_name);
                   break;
    }

    /* get list of variables */
    int ivar, len;
    char name[MCMXNM + 1]; /* +1 because of terminating '\0' */
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

SEXP get_eq_names_c(SEXP model_index_, SEXP status_, SEXP solve_order_,
                    SEXP endo_names_) {
    /* This function returns the names of the equations or,
     * if endo_names == 1, the names of the left hand side variables of the
     * equations.
     * INPUT:
     *   model_index_  the index of the model
     *   status_       status ("all", "active" or "inactive")
     *   solve_order_  0 if the equations should be returned in natural order
     *                 1 if the equations should be returned in solution order
     *   endo_names_   0 if the function should return equation names,
     *                 1 if it should return the names of the lhs variables.
     *                 The equation and lhs names are usually the same,
     *                 but not always.
     */
    int model_index = asInteger(model_index_);
    int endo_names = asInteger(endo_names_);
    int solve_order = asInteger(solve_order_);
    const char *status_str = CHAR(asChar(status_));
    int status;
    if (strcmp(status_str, "all") == 0) {
        status = ALL;
    } else if (strcmp(status_str, "active") == 0) {
        status = ACTIVE;
    } else if (strcmp(status_str, "inactive") == 0) {
        status = INACTIVE;
    } else {
        error("Illegal equation type %s specified\n", status_str);
    }

    if (status != INACTIVE && solve_order == 1) {
       F77_CALL(check_active_eqs_fortran)(&model_index);
    }

    int neq  = F77_CALL(get_eq_count)(&model_index);
    int alphabet = 0;

    /* get list of equation indices */
    int *ieqs = (int *) R_alloc(neq, sizeof(int));
    int cnt = 0;
    int i, ieq, ok;
    for (i = 1; i <= neq; i++) {
        ieq = solve_order ? F77_CALL(get_eq_order)(&model_index, &i) : i;
        if (ieq <= 0) {
            continue;
        }
        if (status == ACTIVE || status == INACTIVE) {
            int is_active = F77_CALL(equation_is_active)(&model_index, &ieq);
            ok = status == ACTIVE ? is_active : !is_active;
        } else {
            ok = 1;
        }
        if (ok) {
            ieqs[cnt++] = ieq;
        }
    }

    /* now create character vector with the selected equation names */
    int len, ivar;
    char name[MCMXNM + 1]; /* +1 because of terminating '\0' */
    SEXP names = PROTECT(allocVector(STRSXP, cnt));
    for (i = 0; i < cnt; i++) {
        if (endo_names) {
            ivar = F77_CALL(get_lhsnum)(&model_index, ieqs + i);
            F77_CALL(get_variable_name)(&model_index, &ivar, name, &len,
                                        &alphabet);
        } else {
            F77_CALL(get_equation_name)(&model_index, ieqs + i, name, &len,
                                    &alphabet);
        }
        name[len] = '\0';
        SET_STRING_ELT(names, i, mkChar(name));
    }
    UNPROTECT(1);
    return names;
}


/* Get the model parameters */
SEXP get_param_c(SEXP model_index_, SEXP names) {
    int model_index = asInteger(model_index_);
    int npar = length(names);
    int *ipar = (int *) R_alloc(npar, sizeof(int));
    int cnt = 0;
    for (int i = 0; i < npar; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int ip = F77_CALL(get_par_index)(&model_index, name, &namelen);
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
        int par_len = F77_CALL(get_param_length)(&model_index, ipar + i);
        SEXP value = PROTECT(allocVector(REALSXP,  par_len));
        F77_CALL(get_param_fortran)(&model_index, ipar + i, REAL(value), &par_len);
        SET_VECTOR_ELT(retval,  i, value);
        UNPROTECT(1);
    }

    setAttrib(retval, R_NamesSymbol, names);
    UNPROTECT(1);
    return retval;
}


/* General function for getting model data or constant adjustments */
SEXP get_data_c(SEXP type_, SEXP model_index_, SEXP names, SEXP jtb_, SEXP jte_) {

    int type = asInteger(type_);

    int model_index = asInteger(model_index_);
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
        int idx = (*get_index)(&model_index, name, &namelen);
        if (idx > 0) {
            ivar[cnt++] = (*get_index)(&model_index, name, &namelen);
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
    SEXP data = PROTECT(create_matrix(ntime, cnt));
    (*get_dat)(&model_index, &cnt, ivar, &ntime, &jtb, &jte, REAL(data));

    UNPROTECT(1);
    return data;
}

/* Sets the model parameters. Returns the number of parameters actually set */
SEXP set_param_c(SEXP model_index_, SEXP param_list) {
    int model_index = asInteger(model_index_);
    SEXP names = getAttrib(param_list, R_NamesSymbol);
    int n_names = length(names);
    int i;
    int cnt = 0;
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int ip = F77_CALL(get_par_index)(&model_index, name, &namelen);
        if (ip > 0) {
            cnt++;
            SEXP value = VECTOR_ELT(param_list, i);
            int len = length(value);
            int ret = F77_CALL(set_param_fortran)(&model_index, &ip, REAL(value),
                               &len);
            if (ret == 1) {
                error("Value for parameter %s has an incorrect length. Required length: %d. Actual length: %d", 
                      name, F77_CALL(get_param_length)(&model_index, &ip), len);
            }
        }
    }
    return ScalarInteger(cnt);
}

/* General function for setting model data, constant adjustments,
 * fix value or fit values */
SEXP set_data_c(SEXP set_type_, SEXP model_index_, SEXP mat, SEXP names,
                SEXP shift_, SEXP upd_mode_) {

    int set_type = asInteger(set_type_);
    int model_index = asInteger(model_index_);
    int shift = asInteger(shift_);
    const char *upd_mode_str = CHAR(asChar(upd_mode_));

    /* Convert upd_mode_str to an integer. The integer
     * values should agree with the parameters
     * UPD and UPD_NA in Fortran module mws_type. */
    int upd_mode = 1;
    if (strcmp(upd_mode_str, "upd") == 0) {
        upd_mode = 1;
    } else if (strcmp(upd_mode_str, "updval") == 0) {
        upd_mode = 3;
    }

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
        int iv = F77_CALL(get_var_index)(&model_index, name, &namelen);
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
            F77_CALL(set_data_fortran)(&model_index, &nvar, ivar, &ntime, &jtb,
                                       &jte, REAL(mat), icol, &upd_mode);
            break;
        case SET_CA:
            F77_CALL(set_ca_fortran)(&model_index, &nvar, ivar, &ntime, &jtb,
                                     &jte, REAL(mat), icol, &upd_mode);
            break;
        case SET_FIX:
        case SET_FIT:
            fix = set_type == SET_FIX;
            F77_CALL(set_fix_fit_fortran)(&model_index, &nvar, ivar, &ntime, &jtb,
                                          &jte, REAL(mat), icol, &fix,
                                          &upd_mode);
            break;
    }
    return R_NilValue;
}

/* General function for getting fix value or fit values */
SEXP get_fix_fit_c(SEXP type_, SEXP model_index_) {
    const char *type_str = CHAR(STRING_ELT(type_, 0));
    int fix = strcmp(type_str, "fix") == 0 ? 1 : 0;
    int model_index = asInteger(model_index_);
    int nvar, jtb, jte;

    if (fix) {
        F77_CALL(get_fix_info)(&model_index, &nvar, &jtb, &jte);
    } else  {
        F77_CALL(get_fit_info)(&model_index, &nvar, &jtb, &jte);
    }

    if (nvar == 0) {
        return R_NilValue;
    }

    int ntime = jte - jtb + 1;
    SEXP mat = PROTECT(create_matrix(ntime, nvar));
    int *ivar = (int *) R_alloc(nvar, sizeof(int));
    F77_CALL(get_fix_fit_fortran)(&model_index, &nvar, ivar, &ntime, &jtb,
                                  REAL(mat), &fix);

    SEXP names = PROTECT(get_variable_names(ivar, nvar, model_index));

    // create ouput list
    SEXP list = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(list, 0, ScalarInteger(jtb));
    SET_VECTOR_ELT(list, 1, mat);
    SET_VECTOR_ELT(list, 2, names);

    UNPROTECT(3);
    return list;
}

SEXP set_rms_c(SEXP model_index_, SEXP values) {
    int model_index = asInteger(model_index_);
    SEXP names = getAttrib(values, R_NamesSymbol);
    int n_names = length(values);
    int i;
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int iv = F77_CALL(get_var_index)(&model_index, name, &namelen);
        if (iv > 0) {
            double value = REAL(values)[i];
            F77_NAME(set_rms_fortran)(&model_index, &iv, &value);
        }
    }
    return R_NilValue;
}

SEXP get_rms_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    if (!F77_CALL(has_rms_fortran)(&model_index)) {
        return R_NilValue;
    } else {
        int n = F77_CALL(get_ca_count)(&model_index);
        SEXP ret = PROTECT(allocVector(REALSXP, n));
        F77_CALL(get_rms_fortran)(&model_index, REAL(ret));
        UNPROTECT(1);
        return ret;
    }
}

/* Extracts the solve Jacobian from Fortran memory. Returns an unprotected SEXP
   holding the Jacobian, or R_NilValue if the Jacobian was not stored */
SEXP get_solve_jac_c(int model_index) {

    /* Determine the dimension of the jacobian:
       nu (number of residuals) and nw (number of observations */
    int nfb = F77_CALL(get_solve_jac_dim)();
    if (nfb == 0) {
        // No feedback variables, or fit option store_jac is FALSE.
        return R_NilValue;
    }

    SEXP jac = PROTECT(create_matrix(nfb, nfb));

    /* Extract variable indices and Jacobian from Fortran memory */
    int *numfb = (int *) R_alloc(nfb, sizeof(int));
    F77_CALL(get_solve_jac_info)(numfb, REAL(jac));

    /* Convert variable indices to names of the feedback variables */
    SEXP fb_names = PROTECT(get_variable_names(numfb, nfb, model_index));

    jac = add_row_and_column_names(jac, fb_names, fb_names);

    UNPROTECT(2);

    /* Return unprotected result */
    return jac;
}

/* Extracts the fit Jacobian from Fortran memory. Returns an unprotected SEXP
   holding the Jacobian, or R_NilValue if no fit Jacobian was available */
SEXP get_fit_jac_c(int model_index) {

    /* Determine the dimension of the jacobian:
       nu (number of residuals) and nw (number of observations */
    int nu, nw;
    F77_CALL(get_fit_jac_dim_wrap)(&nu, &nw);

    if (nu == 0 || nw == 0) {
        // No fit called, or fit option store_jac is FALSE.
        return R_NilValue;
    }

    /* Create jacobian matrix */
    SEXP jac = PROTECT(create_matrix(nw, nu));

    /* Extract variable indices and Jacobian from Fortran memory */
    int *numu = (int *) R_alloc(nu, sizeof(int));
    int *numw = (int *) R_alloc(nw, sizeof(int));
    F77_CALL(get_fit_jac_info_wrap)(numu, numw, REAL(jac), &nu, &nw);

    /* Convert variable indices to variable names */
    SEXP u_names = PROTECT(get_variable_names(numu, nu, model_index));
    SEXP w_names = PROTECT(get_variable_names(numw, nw, model_index));

    jac = add_row_and_column_names(jac, w_names, u_names);

    UNPROTECT(3);

    /* Return unprotected result */
    return jac;
}

SEXP solve_c(SEXP model_index_, SEXP startp_, SEXP endp_, SEXP options,
             SEXP fit_options) {

    // process arguments
    int model_index = asInteger(model_index_);
    int startp = asInteger(startp_);
    int endp= asInteger(endp_);

    /* initiale the solve options in msvars (copy from mws) */
    F77_CALL(msoptsinit_wrap)(&model_index);

    int opts_present = length(options) > 0;
    int fit_opts_present = length(fit_options) > 0;

    if (opts_present || fit_opts_present) {
        int use_mws = 0;
        F77_CALL(init_set_options)(&model_index, &use_mws);
    }

    if (opts_present) {
        set_solve_options(&model_index, options);
    }
    if (fit_opts_present) {
        set_fit_options(&model_index, fit_options);
    }

    /* Now run solve */
    int simerr;
    F77_CALL(solve_fortran)(&model_index, &startp, &endp, &simerr);

    /* Solve Jacobian */
    SEXP solve_jac = PROTECT(get_solve_jac_c(model_index));

    /* Fit Jacobian */
    SEXP fit_jac = PROTECT(get_fit_jac_c(model_index));

    /* allocate list (VECSXP) of length 3 and protect it */
    SEXP ret = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(ret, 0, ScalarInteger(simerr));
    SET_VECTOR_ELT(ret, 1, solve_jac);
    SET_VECTOR_ELT(ret, 2, fit_jac);

    /* allocate character vector for names (STRSXP) and fill it */
    SEXP names = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("simerr"));
    SET_STRING_ELT(names, 1, mkChar("solve_jac"));
    SET_STRING_ELT(names, 2, mkChar("fit_jac"));

    /* attach names attribute */
    setAttrib(ret, R_NamesSymbol, names);

    UNPROTECT(4);

    return ret;
}

SEXP filmdt_c(SEXP model_index_, SEXP startp_, SEXP endp_, SEXP report_,
              SEXP include_frmls_sexp) {
    const char *REPORT_OPTIONS[] = {"no", "minimal", "period"};
    const char *report = CHAR(STRING_ELT(report_, 0));
    int model_index = asInteger(model_index_);
    int startp = asInteger(startp_);
    int endp = asInteger(endp_);
    int report_type = get_i_option("report", report, REPORT_OPTIONS,
                                   NO_ELM(REPORT_OPTIONS));
    int include_frmls_int = asInteger(include_frmls_sexp);
    F77_CALL(filmdt_fortran)(&model_index, &startp, &endp, &report_type, &include_frmls_int);
    return R_NilValue;
}

/* Sets convergence criterium for some model variables */
SEXP set_cvgcrit_c(SEXP model_index_, SEXP names, SEXP value_) {
    int model_index = asInteger(model_index_);
    double value = asReal(value_);
    int i, cnt = 0;
    int n_names =  length(names);
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int iv = F77_CALL(get_var_index)(&model_index, name, &namelen);
        if (iv > 0) {
            cnt++;
            F77_CALL(set_test)(&model_index, &iv, &value);
        } else {
           warning("\"%s\" is not a model variable\n", name);
        }
    }

    if (cnt < n_names) {
        error("%d incorrect variable name(s) encountered. See warning(s).\n",
              n_names - cnt);
    }
    return R_NilValue;
}

/* Sets convergence criterium for all model variables, used in init_mws.
 * values is a vector with convergence criteria for the model variables
 * in natural (i.e. non-alphabetical) order */
SEXP set_cvgcrit_init_mws_c(SEXP model_index_, SEXP values) {
    int model_index = asInteger(model_index_);
    int i;
    for (i = 1; i <= length(values); i++) {
        double value = REAL(values)[i - 1];
        F77_CALL(set_test)(&model_index, &i, &value);
    }
    return R_NilValue;
}

/* Returns the convergence criteria for all model variables */
SEXP get_cvgcrit_c(SEXP model_index_, SEXP alphabet_) {
    int model_index = asInteger(model_index_);
    int alphabet = asInteger(alphabet_);
    int nvar = F77_CALL(get_variable_count)(&model_index);
    SEXP ret = PROTECT(allocVector(REALSXP, nvar));
    int iv;
    for (iv = 1; iv <= nvar; iv++) {
        REAL(ret)[iv - 1] = F77_CALL(get_test)(&model_index, &iv, &alphabet);
    }
    UNPROTECT(1);
    return ret;
}

/* Sets Fair-Taylor relaxtion factors */
SEXP set_ftrelax_c(SEXP model_index_, SEXP names, SEXP value_) {
    int model_index = asInteger(model_index_);
    double value = asReal(value_);
    int i, cnt = 0;
    int n_names =  length(names);
    for (i = 0; i < n_names; i++) {
        const char *name = CHAR(STRING_ELT(names, i));
        int namelen = strlen(name);
        int iendex = F77_CALL(get_iendex)(&model_index, name, &namelen);
        if (iendex > 0) {
            cnt++;
            F77_CALL(set_ftrelax)(&model_index, &iendex, &value);
        } else {
           warning("\"%s\" is not an endogenous lead.\n", name);
        }
    }

    if (cnt < n_names) {
        error("%d incorrect variable name(s) encountered. See warning(s).\n",
              n_names - cnt);
    }
    return R_NilValue;
}

/* Sets Fair-Taylor relaxation factors for all endogenous leads.
 * in natural (i.e. non-alphabetical) order */
SEXP set_ftrelax_init_mws_c(SEXP model_index_, SEXP values) {
    int model_index = asInteger(model_index_);
    int i;
    for (i = 1; i <= length(values); i++) {
        double value = REAL(values)[i - 1];
        F77_CALL(set_ftrelax)(&model_index, &i, &value);
    }
    return R_NilValue;
}

/* Returns the convergence criteria for all model variables */

/* Returns the Fair-Taylor relaxation factors for all endogenous
 * leads (not in alphabetical order). */
SEXP get_ftrelax_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int nendex = F77_CALL(get_endex_count)(&model_index);
    SEXP ret = PROTECT(allocVector(REALSXP, nendex));
    int iendex;
    for (iendex = 1; iendex <= nendex; iendex++) {
        REAL(ret)[iendex - 1] = F77_CALL(get_ftrelax)(&model_index, &iendex);
    }
    UNPROTECT(1);
    return ret;
}


/* Activate/deactive equation */
SEXP set_eq_status_c(SEXP model_index_, SEXP names, SEXP status) {
    int model_index = asInteger(model_index_);
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
        int ieq = F77_CALL(get_eq_index)(&model_index, name, &namelen);
        if (ieq > 0) {
            fun(&model_index, &ieq);
        }
    }
    return R_NilValue;
}

/* Activate all equations */
SEXP activate_all_equations_c(SEXP mdl_index_) {
    int mdl_index = asInteger(mdl_index_);
    int neq  = F77_CALL(get_eq_count)(&mdl_index);
    int ieq;
    for (ieq = 0; ieq < neq; ieq++) {
        F77_CALL(activate_equation)(&mdl_index, &ieq);
    }
    return R_NilValue;
}

SEXP has_free_mws_c(void) {
    int has_free_mws = F77_CALL(has_free_mws_fortran)();
    return ScalarLogical(has_free_mws);
}

SEXP get_max_lag_lead_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int maxlag, maxlead;
    F77_CALL(get_max_lag_lead_fortran)(&model_index, &maxlag, &maxlead);
    SEXP ret = PROTECT(allocVector(INTSXP, 2));
    INTEGER(ret)[0] = maxlag;
    INTEGER(ret)[1] = maxlead;
    UNPROTECT(1);
    return ret;
}

SEXP remove_mws_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    F77_CALL(remove_mws_fortran)(&model_index);
    return R_NilValue;
}

SEXP remove_all_mwss_c(void) {
    F77_CALL(remove_all_mwss_fortran)();
    return R_NilValue;
}

SEXP set_dbgeqn_c(SEXP model_index_, SEXP dbgeqn_) {
    int model_index = asInteger(model_index_);
    int dbgeqn = asLogical(dbgeqn_);
    F77_CALL(set_dbgeqn_fortran)(&model_index, &dbgeqn);
    return R_NilValue;
}

SEXP get_dbgeqn_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int dbgeqn = F77_CALL(get_dbgeqn_fortran)(&model_index);	
    return(ScalarLogical(dbgeqn));
}

SEXP run_eqn_c(SEXP model_index_, SEXP eqnums, SEXP jtb_, SEXP jte_, 
               SEXP updval__, SEXP by_period__) {
    int model_index = asInteger(model_index_);
    int jtb = asInteger(jtb_);
    int jte = asInteger(jte_);
    int updval_ = asInteger(updval__);
    int by_period_ = asInteger(by_period__);
    int neq = length(eqnums);
    F77_CALL(run_eqn_fortran)(&model_index, &neq, INTEGER(eqnums), &jtb, &jte, 
             &updval_, &by_period_);
    return R_NilValue;
}

SEXP set_jc_c(SEXP model_index_, SEXP jc_) {
    int model_index = asInteger(model_index_);
    int jc  = asInteger(jc_);
    F77_CALL(set_jc_fortran)(&model_index, &jc);
    return R_NilValue;
}

SEXP get_jc_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int jc = F77_CALL(get_jc_fortran)(&model_index);	
    return(ScalarInteger(jc));
}

SEXP clear_fit_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    F77_CALL(clear_fit_fortran)(&model_index);
    return R_NilValue;
}

SEXP clear_fix_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    F77_CALL(clear_fix_fortran)(&model_index);
    return R_NilValue;
}

SEXP clone_mws_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int model_index_clone = F77_CALL(clone_mws_fortran)(&model_index);	
    return(ScalarInteger(model_index_clone));
}

SEXP set_period_c(SEXP model_index_, SEXP start, SEXP end, SEXP freq_) {
    int model_index = asInteger(model_index_);
    int freq = asInteger(freq_);
    int ierr = F77_CALL(set_period_fortran)(&model_index, INTEGER(start),
                                           INTEGER(end), &freq);	
    return(ScalarInteger(ierr));
}

/* Returns the names of the simulaneous parameters */
SEXP get_simul_names_c(SEXP model_index_) {
    int model_index = asInteger(model_index_);
    int nsimul = F77_CALL(get_simul_count)(&model_index);

    /* get list of parameters */
    int isimul, len;
    char name[MCMXNM + 1]; /* +1 because of terminating '\0' */
    SEXP names = PROTECT(allocVector(STRSXP, nsimul));
    for (isimul = 1; isimul <= nsimul; isimul++) {
        F77_CALL(get_simul_name)(&model_index, &isimul, name, &len);
        name[len] = '\0';
        SET_STRING_ELT(names, isimul - 1, mkChar(name));
    }
    UNPROTECT(1);
    return names;
}
