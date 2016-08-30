const char *get_mode_text(int imode);
const char *get_start_text(int istart);
const char *get_xupdate_text(int uplead);
int get_imode(const char *mode_text);
int get_istart(const char *stary_text);
int get_uplead(const char *xupdate_text);
int get_erropt(const char *erropt_text);
const char *get_erropt_text(int erropt);

extern const char *DBG_ALL;
extern const char *DBG_NONE;
extern const char *DBG_PRITER_OPTS[];
extern const char *DBG_PREXEN_OPTS[];
extern const char *DBG_JACPRT_OPTS[];
extern const char *DBG_SUPTST_OPTS[];
extern const char *DBG_XSUPTT_OPTS[];
extern const char *DBG_PRSCAL_OPTS[];
