/* C declarations of Fortran subroutines and functions to be called from C */

extern int F77_NAME(get_param_count)(int *model_index);
extern int F77_NAME(get_variable_count)(int *model_index);
extern int F77_NAME(get_ca_count)(int *model_index);
extern int F77_NAME(get_eq_count)(int *model_index);
extern void F77_NAME(get_param_name)(int *model_index, int *i,
                                        char *param_name, int *len);
extern void F77_NAME(get_variable_name)(int *model_index, int *i,
                                        char *variable_name, int *len);
extern void F77_NAME(get_ca_name)(int *model_index, int *i, char *ca_name,
                                  int *len);
extern void F77_NAME(get_equation_name)(int *model_index, int *i, char *name,
                                        int *len, int *alpha);
extern int F77_NAME(get_var_index)(int *mws_index, const char *name, int*namelen);
extern int F77_NAME(get_par_index)(int *mws_index, const char *name, int*namelen);
extern int F77_NAME(get_ca_index)(int *mws_index, const char *name, int*namelen);
extern int F77_NAME(get_eq_index)(int *mws_index, const char *name, int*namelen);
extern void F77_NAME(get_fix_info)(int *mws_index, int *nfix, int *jtb,
                                   int *jte);
extern void F77_NAME(get_fit_info)(int *mws_index, int *nfit, int *jtb,
                                   int *jte);
extern int F77_NAME(get_param_length)(int *model_index, int *i);
extern int F77_NAME(equation_is_active)(int *model_index, int *ieq, int *alpha);
