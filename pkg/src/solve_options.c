#include <string.h>
#include "solve_options.h"
#include "option_utils.h"

/* NOTE: the ordering of MODES, START_OPTIONS etc. should agree with the
 * ordering of the options in solve_options_type.f90 */
static const char *MODES[] = {"dynamic", "ratex", "reschk", "backward", 
                              "static", "auto"};
static const char *START_OPTIONS[] = {"previous", "current", "curifok", 
                                      "previfok"};
static const char *XUPDATE_OPTIONS[] = {"fixed", "lastval"};
const char *DBG_ALL  = "allinfo";
const char *DBG_NONE = "noinfo";
static const char *ERROPT_OPTIONS[] = {"stop", "cont"};
static const char *REP_OPTIONS[] = {"none", "minimal", "period", "fullrep"};
static const char *RATREP_OPTIONS[] = {"minimal", "iter", "fullrep"};

const char *DBG_PRITER_OPTS[] = {"noprifb",       "prifb"};
const char *DBG_PREXEN_OPTS[] = {"noprild",       "prild"};
const char *DBG_JACPRT_OPTS[] = {"noprijac",      "prijac"};
const char *DBG_SUPTST_OPTS[] = {"noprinoconv",   "prinoconv"};
const char *DBG_XSUPTT_OPTS[] = {"noprinotconvl", "prinotconvl"};
const char *DBG_PRSCAL_OPTS[] = {"nopriscal",     "priscal"};

const char *get_mode_text(int imode) {
    return get_option_text(imode, MODES, NO_ELM(MODES));
}

const char *get_start_text(int istart) {
    return get_option_text(istart, START_OPTIONS, NO_ELM(START_OPTIONS));
}

const char *get_xupdate_text(int uplead) {
    return XUPDATE_OPTIONS[uplead];
}

const char *get_erropt_text(int erropt) {
    return get_option_text(erropt, ERROPT_OPTIONS, NO_ELM(ERROPT_OPTIONS));
}

const char *get_repopt_text(int repopt) {
    return get_option_text(repopt, REP_OPTIONS, NO_ELM(REP_OPTIONS));
}

const char *get_ratrepopt_text(int ratrepopt) {
    return get_option_text(ratrepopt, RATREP_OPTIONS, NO_ELM(RATREP_OPTIONS));
}

int get_imode(const char *name, const char *mode_text) {
    return get_i_option(name, mode_text, MODES, NO_ELM(MODES));
}

int get_istart(const char *name, const char *start_text) {
    return get_i_option(name, start_text, START_OPTIONS, NO_ELM(START_OPTIONS));
}

int get_uplead(const char *name, const char *xupdate_text) {
    return get_i_option(name, xupdate_text, XUPDATE_OPTIONS, 
                        NO_ELM(XUPDATE_OPTIONS)) - 1;
}

int get_erropt(const char *name, const char *erropt_text) {
    return get_i_option(name, erropt_text, ERROPT_OPTIONS, 
                        NO_ELM(ERROPT_OPTIONS));
}

int get_repopt(const char *name, const char *repopt_text) {
    return get_i_option(name, repopt_text, REP_OPTIONS, NO_ELM(REP_OPTIONS));
}

int get_ratrepopt(const char *name, const char *ratrepopt_text) {
    return get_i_option(name, ratrepopt_text, RATREP_OPTIONS, 
                        NO_ELM(RATREP_OPTIONS));
}
