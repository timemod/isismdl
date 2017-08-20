/*
 * initialize options
 */

#include <stdio.h>
#include "mcopt.h"

/* mcopt_init: initialise the options for the model compiler */
void mcopt_init(Mcopt *options) {
    options->McIsisMdl = 1;
    options->Showhash  = 0;
    options->Showecode = 0;
    options->Showocode = 0;
    options->Makezrf   = 0;
    options->gen_dep   = 0;
    options->Substufunc= 0;
    options->MakeTroll = 0;
    options->MakeEviews= 0;
    options->Strict    = 0;
    options->mdlname   = NULL;
    options->outputname  = NULL;
}
