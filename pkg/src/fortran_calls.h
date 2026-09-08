#ifndef FORTRAN_CALLS_H
#define FORTRAN_CALLS_H

extern void F77_NAME(get_max_lag_lead_fortran)(int *model_index, int *maxlag, int *maxlead);
extern int F77_NAME(set_period_fortran)(int *model_index, int *start, int *end, int *freq);
extern void F77_NAME(get_data_fortran)(int *model_index, int *nvar, int *ivar,
                                       int *ntime, int *jtb,
                                       int *jte, double *mat);
extern void F77_NAME(get_ca_fortran)(int *model_index, int *nca, int *ica,
                                     int *ntime, int *jtb,
                                     int *jte, double *mat);
extern void F77_NAME(get_fix_fit_fortran)(int *model_index, int *nvar, int *ivar,
                                          int *ntime, int *jtb, double *mat,
                                          int *fix_);
extern void F77_NAME(get_param_fortran)(int *model_index, int *ipar, double *value,
                                        int *length);
extern int F77_NAME(set_param_fortran)(int *model_index, int *ipar, double *value,
                                       int *length);
extern void F77_NAME(set_data_fortran)(int *model_index, int *nvar, int *ivar,
                                       int *ntime, int *jtb, int *jte,
                                       double *mat, int *icol, int *upd_mode);
extern void F77_NAME(set_ca_fortran)(int *model_index, int *nvar, int *ivar,
                                     int *ntime, int *jtb, int *jte,
                                     double *mat, int *icol, int *upd_mode);
extern void F77_NAME(set_fix_fit_fortran)(int *model_index, int *nvar, int *ivar,
                                          int *ntime, int *jtb, int *jte,
                                          double *mat, int *icol, int *fix_,
                                          int *upd_mode);
extern void F77_NAME(set_rms_fortran)(int *model_index, int *var_index,
                                      double *value);
extern int F77_NAME(has_rms_fortran)(int *model_index);
extern void F77_NAME(get_rms_fortran)(int *model_index, double *values);
extern void F77_NAME(run_eqn_fortran)(int *model_index, int *neq, int *eqnums,
                                      int *jtb, int *jte, int *updval_,
                                      int *by_period_);
extern void F77_NAME(msoptsinit_wrap)(int *model_index);
extern void F77_NAME(solve_fortran)(int *model_index, int *jtb, int *jte, int *error);
extern void F77_NAME(filmdt_fortran)(int *model_index, int *jtb, int *jte,
                                     int *report_type, int *include_frmls_int);
extern void F77_NAME(remove_mws_fortran)(int *model_index);
extern void F77_NAME(remove_all_mwss_fortran)(void);
extern void F77_NAME(clear_fit_fortran)(int *model_index);
extern void F77_NAME(clear_fix_fortran)(int *model_index);
extern void F77_NAME(set_test)(int *model_index, int *ivar, double *value);
extern double F77_NAME(get_test)(int *model_index, int *ivar, int *alphabet);
extern double F77_NAME(get_ftrelax)(int *model_index, int *iendex);
extern void F77_NAME(activate_equation)(int *model_index, int *eqnum);
extern void F77_NAME(deactivate_equation)(int *model_index, int *eqnum);
extern void F77_NAME(set_ftrelax)(int *model_index, int *ivar, double *value);
extern int F77_NAME(has_free_mws_fortran)(void);
extern int F77_NAME(clone_mws_fortran)(int *model_index);
extern void F77_NAME(set_dbgeqn_fortran)(int *model_index, int *dbgeqn_);
extern int F77_NAME(get_dbgeqn_fortran)(int *model_index);
extern void F77_NAME(set_jc_fortran)(int *model_index, int *jc);
extern int F77_NAME(get_jc_fortran)(int *model_index);
extern void F77_NAME(check_active_eqs_fortran)(int *model_index);
extern void F77_NAME(get_fit_jac_dim_wrap)(int *nu, int *nw);
extern void F77_NAME(get_fit_jac_info_wrap)(int *numu, int *numw, double *jac,
                                            int *nu, int *nw);

#endif /* FORTRAN_CALLS_H */
