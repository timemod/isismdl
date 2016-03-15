#ifndef MCOPT_H
#define MCOPT_H

typedef struct _mcopt {
    int Showecode;
    int Showocode;
    int Substufunc;
    int Showhash;
    int MakeTroll;
    int MakeEviews;
    int Debug;
    int Strict;    /* 1 for strict compilation: parameters defined before use */
    int gen_dep; /* 1 to generate a file with full dependency information */
    char *mdlname;
    char *outputname; /* the names of the output files exclusing extenstion */
}
Mcopt;

void mcopt_init(Mcopt *options);

#endif
