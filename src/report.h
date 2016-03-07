extern int mws_index;

void init_report(int mws_index, const char *period_string);
char *get_solve_summary(void);
char *close_report(void);
void rep_printf(const char *format, ...);
