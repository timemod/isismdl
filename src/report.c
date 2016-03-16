#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <R.h>

#include "report.h"

// TODO: for the time being, make the size of report_text very big
// Later may it smaller (1024 or so).
#define INITIAL_SIZE 1024000

static char *report_text = NULL;
static char *pos;
static size_t size;

char *summary;

void init_report(void) {
    if (report_text == NULL) {
        size = INITIAL_SIZE;
        report_text = malloc(size);
        *report_text = '\0';
    }
    pos = report_text;
}

void init_solve_report(const char *period_string) {

    summary = "Solved";

    init_report();
        
    rep_printf("Simulation Period: %s\n", period_string);
    rep_printf("Solve options\n...\nblablabla\n...\n");
}

char *close_report(void) {
    return report_text;
}

void rep_printf(const char *format, ...) {
    va_list arg;
    va_start(arg, format);

    /* if arguments have been specified, then it is not possible
     * to predict the length of the string printed by the followinf
     * vsprinf call. Therfore, we assume that the maximum length is 
     * 256. */
    if (pos + 257 - report_text > size) {
        // Note: 1 extra character for terminating '\0' included.
        // see function close_report
        size *= 2;
        int ipos = pos - report_text;
        report_text = realloc(report_text, size);
        pos = report_text + ipos;
    }
    
    int len = vsprintf(pos, format, arg);
    pos += len;

    /*
    int len = strlen(string);
    if (pos + len - report_text > size) {
        // Note: 1 extra character for terminating '\0' included.
        // see function close_report
        size *= 2;
        int ipos = pos - report_text;
        report_text = realloc(report_text, size);
        pos = report_text + ipos;
    }
    memcpy(pos, string, len);
    pos += len;
    */
}

// add ascii string str to the report, as a separate line
void F77_SUB(report_str)(char *str) {
    rep_printf("%s\n", str);
}

void F77_SUB(report_solve_error)(int *error) {
     // create summary of solve problem
     switch(*error) {
         case 1: summary = "Simulation not possible"; break;
         case 2: summary = "Simulation stopped"; break;
         case 3: summary = "Initial lags/leads missing/invalid. Simulation not possible."; break;
         case 4: summary = "Invalid parameter values detected. Simulation not possible"; break;
         case 5: summary = "Fair-Taylor has not converged"; break;
         case 6: summary = "Out of memory"; break;
         default: summary = "Unknown problem in solve"; break;
     }
     // write summary to the full report
     rep_printf("%s\n", summary);
}

char *get_summary(void) {
    return summary;
}
