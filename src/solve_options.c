#include <string.h>
#include "solve_options.h"

/* NOTE: the ordering of MODES and START_OPTIONS should agree with the
 * ordering of the options in solve_options_type.f90 */
static const char *MODES[] = {"dynamic", "ratex", "reschk", "backward", 
                              "static"};
static const char *START_OPTIONS[] = {"previous", "current", "curifok", 
                                      "previfok"};
static const char *XUPDATE_OPTIONS[] = {"fixed", "lastval"};
const char *DBG_ALL  = "allinfo";
const char *DBG_NONE = "noinfo";
static const char *ERROPT_OPTIONS[] = {"stop", "cont"};
static const char *REP_OPTIONS[] = {"none", "minimal", "period", "fullrep"};
static const char *RATREP_OPTIONS[] = {"minimal", "iter", "iterscrn",
                                       "fullrep", "fullrepscrn"};

const char *DBG_PRITER_OPTS[] = {"noprifb",       "prifb"};
const char *DBG_PREXEN_OPTS[] = {"noprild",       "prild"};
const char *DBG_JACPRT_OPTS[] = {"noprijac",      "prijac"};
const char *DBG_SUPTST_OPTS[] = {"noprinoconv",   "prinoconv"};
const char *DBG_XSUPTT_OPTS[] = {"noprinotconvl", "prinotconvl"};
const char *DBG_PRSCAL_OPTS[] = {"nopriscal",     "priscal"};

#define NO_ELM(x) (sizeof(x) / sizeof(char *))

static const char *get_option_text(int i_option, const char *options[],
                                   int option_count) {
    static const char *UNKNOWN = "???";
    if (i_option < 1 || i_option > option_count) {
       return UNKNOWN;
    } else {
        return options[i_option - 1];
    }
} 

static int get_i_option(const char *option_text, const char *options[],
                        int option_count) {
    int i;
    for (i = 0; i < option_count; i++) {
        if (strcmp(option_text, options[i]) == 0) {
            return i + 1;
        }
    }
    return NO_ELM(options);  /* ERROR: option = ??? */
}


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

int get_imode(const char *mode_text) {
    return get_i_option(mode_text, MODES, NO_ELM(MODES));
}

int get_istart(const char *start_text) {
    return get_i_option(start_text, START_OPTIONS, NO_ELM(START_OPTIONS));
}

int get_uplead(const char *xupdate_text) {
    return strcmp(xupdate_text, XUPDATE_OPTIONS[0]);
}

int get_erropt(const char *erropt_text) {
    return get_i_option(erropt_text, ERROPT_OPTIONS, NO_ELM(ERROPT_OPTIONS));
}

int get_repopt(const char *repopt_text) {
    return get_i_option(repopt_text, REP_OPTIONS, NO_ELM(REP_OPTIONS));
}

int get_ratrepopt(const char *ratrepopt_text) {
    return get_i_option(ratrepopt_text, RATREP_OPTIONS, NO_ELM(RATREP_OPTIONS));
}
