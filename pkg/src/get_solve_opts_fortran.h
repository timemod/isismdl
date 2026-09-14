extern void F77_CALL(get_solve_options)(int *imode, int *istart, int *maxit,
    int *maxmat, double *rlxspeed, double *rlxmin, double *rlxmax,
    double *cstpbk, double *cnmtrx, double *xrelax, int *mratex,
    int *uplead, int *erropt, int *repopt, int *ratrepopt,
    int *ratreport_rep, int *ratfullreport_rep, int *bktmax,
    double *xtfac, int *store_jac);

extern void F77_CALL(get_solve_dbgopts)(int *priter, int *prexen, 
    int *jacprt,  int *sutst, int *suptt, int *prscal);

