#ifndef MCOPT_H
#define MCOPT_H

typedef struct _mcopt {
    int McIsisMdl; /* 1 to generate equation code */
    int McPreproc; /* 1 to write the preprocessed mdl to a file */
    int Showecode;
    int Showocode;
    int Substufunc;
    int Showhash;
    int ShowTiming; /* 1 to print timing information */
    int Makezrf;
    int MakeTroll;
    int MakeEviews;
    int MakeDynare;
    int Debug;
    int Strict;    /* 1 for strict compilation: parameters defined before use */
    int gen_dep; /* 1 to generate a file with full dependency information */
}
Mcopt;

void mcopt_init(Mcopt *options);

#endif
