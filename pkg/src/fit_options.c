#include <string.h>
#include "solve_options.h"
#include "option_utils.h"
#include "fit_options.h"

/* NOTE: the ordering of REPORT_OPTIONS should agree with the
 * ordering of the fit%repopt in solve_options_type.f90 */
static const char *FITREP_OPTIONS[] = {"minimal", "fullrep"};
const char *FIT_PRICA_OPTS[] =  {"noprica",     "prica"};
const char *FIT_PRIJAC_OPTS[] = {"noprijac",    "prijac"};
const char *FIT_SUPSOT_OPTS[] = {"nosupsot",    "supsot"};
const char *FIT_SCALE_METHODS[] = {"none", "row"};

const char *get_fit_repopt_text(int repopt) {
    return get_option_text(repopt, FITREP_OPTIONS, NO_ELM(FITREP_OPTIONS));
}

int get_fit_repopt(const char *name, const char *repopt_text) {
    return get_i_option(name, repopt_text, FITREP_OPTIONS, 
                        NO_ELM(FITREP_OPTIONS));
}

const char *get_fit_scale_method_text(int scale_method) {
   return get_option_text(scale_method, FIT_SCALE_METHODS, NO_ELM(FIT_SCALE_METHODS));
}

int get_fit_scale_method(const char *name, const char *scale_method_text) {
    return get_i_option(name, scale_method_text, FIT_SCALE_METHODS, 
                        NO_ELM(FIT_SCALE_METHODS));
}
