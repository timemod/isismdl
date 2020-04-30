#define SET_DATA 1
#define SET_CA   2 
#define SET_FIX  3
#define SET_FIT  4


void init_modules_c(void);
SEXP read_mdl_c(SEXP filename);
void write_mdl_c(SEXP filename, SEXP model_index_);
SEXP get_par_names_c(SEXP model_index_);
SEXP get_var_names_c(SEXP type_, SEXP model_index_);
SEXP get_eq_names_c(SEXP model_index_, SEXP status_, SEXP order_,
                    SEXP endo_names_);
SEXP get_param_c(SEXP model_index_, SEXP names);
SEXP get_data_c(SEXP type_, SEXP model_index_, SEXP names, SEXP jtb_, SEXP jte_);
SEXP set_param_c(SEXP model_index_, SEXP param_list);
void set_data_c(SEXP set_type_, SEXP model_index_, SEXP mat, SEXP names,
                SEXP shift_, SEXP upd_mode_);
SEXP get_fix_fit_c(SEXP type_, SEXP model_index_);
void set_rms_c(SEXP model_index_, SEXP values);
SEXP get_rms_c(SEXP model_index_);
void solve_c(SEXP model_index_, SEXP startp_, SEXP endp_, SEXP options,
             SEXP fit_options);
void filmdt_c(SEXP model_index_, SEXP startp_, SEXP endp_, SEXP report_);
void set_cvgcrit_c(SEXP model_index_, SEXP names, SEXP value_);
void set_cvgcrit_init_mws_c(SEXP model_index_, SEXP values);
SEXP get_cvgcrit_c(SEXP model_index_, SEXP alphabet_);
void set_ftrelax_c(SEXP model_index_, SEXP names, SEXP value_);
void set_ftrelax_init_mws_c(SEXP model_index_, SEXP values);
SEXP get_ftrelax_c(SEXP model_index_);
void set_eq_status_c(SEXP model_index_, SEXP names, SEXP status);
void activate_all_equations_c(SEXP mdl_index_);
SEXP get_solve_status_c(SEXP model_index_);
SEXP has_free_mws_c(void);
SEXP get_max_lag_lead_c(SEXP model_index_);
void remove_mws_c(SEXP model_index_);
void set_dbgeqn_c(SEXP model_index_, SEXP dbgeqn_);
SEXP get_dbgeqn_c(SEXP model_index_);
void run_eqn_c(SEXP model_index_, SEXP eqnums, SEXP jtb_, SEXP jte_);
void set_jc_c(SEXP model_index_, SEXP jc_);
SEXP get_jc_c(SEXP model_index_);
void mdlpas_c(SEXP model_index_, SEXP jtb_, SEXP jte_);
void clear_fit_c(SEXP model_index_);
void clear_fix_c(SEXP model_index_);
SEXP clone_mws_c(SEXP model_index_);
SEXP set_period_c(SEXP model_index_, SEXP start, SEXP end, SEXP freq_);
void remove_mws_c(SEXP model_index_);
