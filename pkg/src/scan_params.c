/*
 * Scans the model for parameters and stores the parameters in the symbol table.
 *
 * In Isis model files, it is allowed to use parameters before they are defined.
 * Therefore it is required to add the parameters to the symbol table before
 * the actual compilation starts. This is performed in the function scan_params.
 * It also counts the number of values for each parameter, and it determines
 * the parameters index. It does NOT store yet the values for the parameters,
 * this is performed by the model compiler.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include "symbol.h"
#include "ecode.h"
#include "mchdr.h"
#include "util.h"
#include "xpcdef.h"
#include "xpctab.h"
#include "isismdl.h"

static void make_param(Symbol *sp, int count);
static void scan_param_statement(void);

static int parCount = 0;

void mc_scan_params(void) 
{
    parCount = 0;
    int token;
    while ((token = mclex()) > 0 && token != T_END) {
        if (token == T_PARAM) {
            scan_param_statement();
        }
     }
}

static void scan_param_statement(void) 
{
    Symbol *sp;
    int token = mclex();
    while (token > 0 && token != ';')  {
        if (token == T_NAME) {
            sp = mclval.sp;
            int count= 0;
            while ((token = mclex()) > 0 && token != T_NAME && token != ';') 
                if (token == T_NUMBER || token == T_INTNUM) count++;
            make_param(sp, count);
        } else {
            /* syntax error. ignore here, the error will be detected by 
             * compilation (next stage). move to next token
             */
            token = mclex();
            WARN("Error scanning parameters.\n");
        }

    }
}

static void make_param(Symbol *sp, int coefCount) 
{
    char *pname = sp->name;

    if( sp->xpctype == XP_PARAM ) 
         mcerror( "Duplicate parameter name %s", sp->name);

    if( strlen(pname) > MAX_PARNLEN )
        mcerror("Parameter name %s too long (max %d chars)",
                    pname, MAX_PARNLEN );

    sp->xpctype = XP_PARAM;

    Param *p = emalloc(sizeof(Param));
    sp->u.parp = p;
    p->cnt = coefCount;
    p->par_index = parCount++;

    return;

}
