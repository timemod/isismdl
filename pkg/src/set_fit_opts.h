#ifndef SET_FIT_OPTS_H
#define SET_FIT_OPTS_H

extern void F77_SUB(set_fit_maxit)(int *);
extern void F77_SUB(set_fit_cvgabs)(double *);
extern void F77_SUB(set_fit_mkdcrt)(double *);
extern void F77_SUB(set_fit_cvgrel)(double *);
extern void F77_SUB(set_fit_zero_ca)(int *);
extern void F77_SUB(set_fit_warn_ca)(int *);
extern void F77_SUB(set_fit_accurate_jac)(int *);
extern void F77_SUB(set_fit_zealous)(int *);
extern void F77_SUB(set_fit_warn_zero_row)(int *);
extern void F77_SUB(set_fit_warn_zero_col)(int *);
extern void F77_SUB(set_fit_dbgopts)(int *prica, int *prijac, int *supsot);
extern void F77_SUB(set_fit_repopt)(int *);
extern void F77_SUB(set_fit_scale_method)(int *);
extern void F77_SUB(set_fit_chkjac)(int *);
extern void F77_SUB(set_fit_store_jac)(int *);

#endif
