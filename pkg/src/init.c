#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#include "c_calls.h"
#include "compile_mdl_c.h"
#include "convert_mdl_file_c.h"
#include "gen_dep_file.h"
#include "get_fit_opts_c.h"
#include "get_solve_opts_c.h"
#include "order_mdl_c.h"
#include "set_fit_opts_c.h"
#include "set_solve_opts_c.h"

static const R_CMethodDef CEntries[] = {
    {"init_modules_c", (DL_FUNC) &init_modules_c, 0},
    {"remove_all_mwss_c", (DL_FUNC) &remove_all_mwss_c, 0},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"read_mdl_c", (DL_FUNC) &read_mdl_c, 2},
    {"write_mdl_c", (DL_FUNC) &write_mdl_c, 2},
    {"get_par_names_c", (DL_FUNC) &get_par_names_c, 1},
    {"get_var_names_c", (DL_FUNC) &get_var_names_c, 2},
    {"get_eq_names_c", (DL_FUNC) &get_eq_names_c, 4},
    {"get_param_c", (DL_FUNC) &get_param_c, 2},
    {"get_data_c", (DL_FUNC) &get_data_c, 5},
    {"set_param_c", (DL_FUNC) &set_param_c, 2},
    {"set_data_c", (DL_FUNC) &set_data_c, 6},
    {"get_fix_fit_c", (DL_FUNC) &get_fix_fit_c, 2},
    {"set_rms_c", (DL_FUNC) &set_rms_c, 2},
    {"get_rms_c", (DL_FUNC) &get_rms_c, 1},
    {"solve_c", (DL_FUNC) &solve_c, 5},
    {"filmdt_c", (DL_FUNC) &filmdt_c, 4},
    {"set_cvgcrit_c", (DL_FUNC) &set_cvgcrit_c, 3},
    {"set_cvgcrit_init_mws_c", (DL_FUNC) &set_cvgcrit_init_mws_c, 2},
    {"get_cvgcrit_c", (DL_FUNC) &get_cvgcrit_c, 2},
    {"set_ftrelax_c", (DL_FUNC) &set_ftrelax_c, 3},
    {"set_ftrelax_init_mws_c", (DL_FUNC) &set_ftrelax_init_mws_c, 2},
    {"get_ftrelax_c", (DL_FUNC) &get_ftrelax_c, 1},
    {"set_eq_status_c", (DL_FUNC) &set_eq_status_c, 3},
    {"activate_all_equation_c", (DL_FUNC) &activate_all_equations_c, 1},
    {"get_solve_status_c", (DL_FUNC) &get_solve_status_c, 1},
    {"has_free_mws_c", (DL_FUNC) &has_free_mws_c, 0},
    {"get_max_lag_lead_c", (DL_FUNC) &get_max_lag_lead_c, 1},
    {"remove_mws_c", (DL_FUNC) &remove_mws_c, 1},
    {"set_dbgeqn_c", (DL_FUNC) &set_dbgeqn_c, 2},
    {"get_dbgeqn_c", (DL_FUNC) &get_dbgeqn_c, 1},
    {"run_eqn_c", (DL_FUNC) &run_eqn_c, 6},
    {"set_jc_c", (DL_FUNC) &set_jc_c, 2},
    {"get_jc_c", (DL_FUNC) &get_jc_c, 1},
    {"clear_fit_c", (DL_FUNC) &clear_fit_c, 1},
    {"clear_fix_c", (DL_FUNC) &clear_fix_c, 1},
    {"clone_mws_c", (DL_FUNC) &clone_mws_c, 1},
    {"set_period_c", (DL_FUNC) &set_period_c, 4},
    {"compile_mdl_c", (DL_FUNC) &compile_mdl_c, 5},
    {"convert_mdl_file_c", (DL_FUNC) &convert_mdl_file_c, 5},
    {"gen_dep_file", (DL_FUNC) &gen_dep_file, 2},
    {"get_fit_opts_c", (DL_FUNC) &get_fit_opts_c, 1},
    {"get_solve_opts_c", (DL_FUNC) &get_solve_opts_c, 1},
    {"order_mdl_c", (DL_FUNC) &order_mdl_c, 2},
    {"set_fit_opts_c", (DL_FUNC) &set_fit_opts_c, 2},
    {"set_solve_opts_c", (DL_FUNC) &set_solve_opts_c, 2},
    {"get_simul_names_c", (DL_FUNC) &get_simul_names_c, 1},
    {NULL, NULL, 0}
};

void R_init_isismdl(DllInfo *dll) {
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
