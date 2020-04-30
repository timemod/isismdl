#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#include "c_calls.h"
#include "compile_mdl_c.h"
#include "convert_mdl_file_c.h"
#include "get_fit_opts_c.h"
#include "get_solve_opts_c.h"
#include "order_mdl_c.h"
#include "set_fit_opts_c.h"
#include "set_solve_opts_c.h"

static const R_CMethodDef CEntries[] = {
    {"init_modules", (DL_FUNC) &init_modules, 0},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"read_mdl_c", (DL_FUNC) &read_mdl_c, 1},
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
    {"set_cvgcrit_init_mws", (DL_FUNC) &set_cvgcrit_init_mws, 2},
    {"get_cvgcrit_c", (DL_FUNC) &get_cvgcrit_c, 2},
    {"set_ftrelax_c", (DL_FUNC) &set_ftrelax_c, 3},
    {"set_ftrelax_init_mws", (DL_FUNC) &set_ftrelax_init_mws, 2},
    {"get_ftrelax_c", (DL_FUNC) &get_ftrelax_c, 1},
    {"set_eq_status_c", (DL_FUNC) &set_eq_status_c, 3},
    {"activate_all_equation_c", (DL_FUNC) &activate_all_equations, 1},
    {"get_solve_status_c", (DL_FUNC) &get_solve_status_c, 1},
    {"has_free_mws", (DL_FUNC) &has_free_mws, 0},
    {"get_max_lag_lead", (DL_FUNC) &get_max_lag_lead, 1},
    {"remove_mws", (DL_FUNC) &remove_mws, 1},
    {"set_dbgeqn", (DL_FUNC) &set_dbgeqn, 2},
    {"get_dbgeqn", (DL_FUNC) &get_dbgeqn, 1},
    {"run_eqn", (DL_FUNC) &run_eqn, 4},
    {"set_jc", (DL_FUNC) &set_jc, 2},
    {"get_jc", (DL_FUNC) &get_jc, 1},
    {"mdlpas", (DL_FUNC) &mdlpas, 3},
    {"clear_fit", (DL_FUNC) &clear_fit, 1},
    {"clear_fix", (DL_FUNC) &clear_fix, 1},
    {"clone_mws", (DL_FUNC) &clone_mws, 1},
    {"set_period", (DL_FUNC) &set_period, 4},
    {"remove_mws_c", (DL_FUNC) &remove_mws_c, 1},
    {"compile_mdl_c", (DL_FUNC) &compile_mdl_c, 5},
    {"convert_mdl_file_c", (DL_FUNC) &convert_mdl_file_c, 5},
    {"get_fit_opts_c", (DL_FUNC) &get_fit_opts_c, 1},
    {"get_solve_opts_c", (DL_FUNC) &get_solve_opts_c, 1},
    {"order_mdl_c", (DL_FUNC) &order_mdl_c, 2},
    {"set_fit_opts_c", (DL_FUNC) &set_fit_opts_c, 2},
    {"set_solve_opts_c", (DL_FUNC) &set_solve_opts_c, 2},
    {NULL, NULL, 0}
};

void R_init_isismdl(DllInfo *dll) {
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
