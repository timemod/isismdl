#ifndef SET_SOLVE_OPTS_H
#define SET_SOLVE_OPTS_H

extern void F77_SUB(set_mode)(int *);
extern void F77_SUB(set_start)(int *);
extern void F77_SUB(set_maxit)(int *);
extern void F77_SUB(set_maxmat)(int *);
extern void F77_SUB(set_rlxspeed)(double *);
extern void F77_SUB(set_rlxmin)(double *);
extern void F77_SUB(set_rlxmax)(double *);
extern void F77_SUB(set_cstpbk)(double *);
extern void F77_SUB(set_cnmtrx)(double *);
extern void F77_SUB(set_xrelax)(double *);
extern void F77_SUB(set_mratex)(int *);
extern void F77_SUB(set_uplead)(int *);
extern void F77_SUB(set_solve_dbgopts)(int *, int *, int *, int *, int *, int *);
extern void F77_SUB(set_erropt)(int *);
extern void F77_SUB(set_repopt)(int *);
extern void F77_SUB(set_ratrepopt)(int *);
extern void F77_SUB(set_ratreport_rep)(int *);
extern void F77_SUB(set_ratfullreport_rep)(int *);
extern void F77_SUB(set_bktmax)(int *);
extern void F77_SUB(set_xtfac)(double *);
extern void F77_SUB(set_store_jac)(int *store_jac);
extern void F77_SUB(check_options)(void);

#endif
