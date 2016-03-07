/* C declarations of Fortran subroutines and functions to be called from C */

extern int F77_NAME(get_variable_count)(int *model_index);
extern void F77_NAME(get_period_info)(int *model_index, int *per_len,
                                      int *max_lag, int *max_lead);
extern void F77_NAME(get_variable_name)(int *model_index, int *i,
                                        char *variable_name, int *len);
extern int F77_NAME(get_var_index)(int *mws_index, const char *name, int*namelen);
