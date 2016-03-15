/*
 * xpc definitions
 */


#ifndef XPCDEF_H
#define XPCDEF_H

/*
 * the following codes are used for the xpctype field
 * of the symbol table entry
 */

#define XP_UNDEF        -1

/*
 * equation types
 */

#define XP_FRML         1
#define XP_IDENT        2
#define XP_IMPLICIT     3

/*
 * other types
 */

/*
 * for commands such as frml ident param
 */

#define XP_CMD          0

/*
 * for model variables
 */

#define XP_PARAM        1       /* parameter    */
#define XP_ENDO         2       /* endogenous   */
#define XP_EXO          3       /* exogenous    */
#define XP_SVAR         4       /* sum index    */
#define XP_FNARG        5       /* function arg */

#define XP_FUNC         6       /* user function*/

#define XP_EQN          7       /* equation     */
#define XP_ULFUNC       8       /* user language function */

/*
 * builtin functions with 1 argument
 */

#define XP_BABS        10
#define XP_BATAN       11
#define XP_BCOS        12
#define XP_BEXP        13
#define XP_BLOG        14
#define XP_BLOG10      15
#define XP_BNINT       16
#define XP_BSIN        17
#define XP_BSQRT       18
#define XP_BTAN        19
#define XP_BTANH       20
#define XP_BASIN       21
#define XP_BACOS       22
#define XP_BSINH       23
#define XP_BCOSH       24
#define XP_BTOREAL     25
#define XP_BCUMNOR     26
#define XP_BINVNOR     27

#define XP_builtin1(x)  ( (x) >= XP_BABS && (x) <= XP_BINVNOR)

/*
 * builtin hypot/fibur functions with 2 arguments
 */

#define XP_BHYPOT      28
#define XP_BFIBUR      29

#define XP_builtin2(x)  ( (x) == XP_BHYPOT || (x) == XP_BFIBUR )

/*
 * builtin min max functions with at least 2 arguments
 */

#define XP_builtin2p(x)  ( (x) == XP_BMIN || (x) == XP_BMAX )

#define XP_BMIN        30
#define XP_BMAX        31

/*
 * maximum parameter vector length
 */

#define  MAXPARAM 256

/*
 * maximum name lengths
 */

#define MAX_UFNNLEN      60
#define MAX_VARNLEN      64
#define MAX_PARNLEN      60

#endif
