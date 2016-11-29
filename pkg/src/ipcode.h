
/*
 * Operator constants for the inverse polish code.
 * The constants should be the same as in src/libf90/mcedef.rf9
 * TODO: generate mcedef.rf9 based on ipcode.h, or vice versa,
 * with a Python script.
 */

#ifndef IPCODE_H
#define IPCODE_H

#define     IP_OPAR    0   // opening parenthesis
#define     IP_LIRL    1   // load immediate real
#define     IP_LISUM   2   // load immediate sumvariable
#define     IP_LIRL8   3   //   load immediate real*8

// load immediate parameter
#define     IP_LIP0    9
#define     IP_LIP1    10
#define     IP_LIP2    11
#define     IP_LIP3    12
#define     IP_LIP4    13
#define     IP_LIP5    14

// load immediate variable
#define     IP_LIV0    17
#define     IP_LIV1    18
#define     IP_LIV2    19
#define     IP_LIV3    20
#define     IP_LIV4    21
#define     IP_LIV5    22

// load immediate formal argument of user function
#define     IP_LIF0    25
#define     IP_LIF1    26
#define     IP_LIF2    27
#define     IP_LIF3    28
#define     IP_LIF4    29
#define     IP_LIF5

#define     IP_NOT     65  // .not.
#define     IP_NEG     73  // unary minus
#define     IP_LOG     74   //natural logarithm
#define     IP_EXP     75  // exp function
#define     IP_ABS     76  // absolute value
#define     IP_SIN     77  // sine
#define     IP_COS     78  // cosine
#define     IP_ATAN    79  // arc tangent
#define     IP_SQRT    80  // square root
#define     IP_NINT    81  // round to integer
#define     IP_LOG10   82  // logarithm base 10
#define     IP_TAN     83  // tangent
#define     IP_ASIN    84  // arcsine
#define     IP_ACOS    85  // arccosine
#define     IP_SINH    86  // hyperbolic sine
#define     IP_COSH    87  // hyperbolic cosine
#define     IP_TANH    88  // hyperbolic tangent
#define     IP_TORL    89  // convert logical to real (true ==> 1 and false ==> 0)
#define     IP_ISVL    90  // isvalid (test if number is not NA) THIS OPCODE IS NO LONGER USED
#define     IP_ABSZ    91  // special absolute value (for Javelin models).
#define     IP_CUMNOR  92   // cumnor function
#define     IP_INVNOR  93   // invnor function
#define     IP_HYPOT   110  // hypot function
#define     IP_HYPOT1  111  // second opcode for hypot function (used internally)
#define     IP_FIBUR   112  // fibur function
#define     IP_FIBUR1  113  // second opcode for fibur function (used internally)
#define     IP_ADD     129  // addition
#define     IP_SUB     130  // subtraction
#define     IP_MUL     131  // multiplication
#define     IP_DIV     132  // division
#define     IP_POW     133  // exponentiation
#define     IP_MAX     134  // maximum
#define     IP_MIN     135  // minimum
#define     IP_LT      137  // .lt.
#define     IP_GT      138  // .gt.
#define     IP_EQ      139  // .eq.
#define     IP_LE      140  // .le.
#define     IP_GE      141  // .ge.
#define     IP_NE      142  // .ne.
#define     IP_OR      145  // .or.
#define     IP_AND     146  // .and.
#define     IP_ADDZ    147  //   addition        NA converted to 0
#define     IP_SUBZ    148  //   subtraction     NA converted to 0
#define     IP_MULZ    149  //   multiplication  NA converted to 1
#define     IP_DIVZ    150  //   division        NA converted to 1

/* Note: ip-codes IP_LTZ .. E_NEZ (151-156) are used internally in the
 * model solve code of Isis. When read_mdl has been called with
 * options = javmdl, then for example IP_LT is replaced by E_LTZ
 * in src/libf7/msjvmd.rf7.
 */

#define     IP_IF      193  //   if
#define     IP_ELSE    194  //   else
#define     IP_ENDIF   195  // endif
#define     IP_SUM     196  // start sum
#define     IP_ENDSUM  197  // end sum
#define     IP_DEL     198  // start del
#define     IP_ENDDEL  199  // end del
#define     IP_CALL    200  // call user statement function
#define     IP_ULCALL  207  // call Isis user language function

// load adress of a parameter
#define     IP_LAP0    201
#define     IP_LAP1    202
#define     IP_LAP2    203
#define     IP_LAP3    204
#define     IP_LAP4    205
#define     IP_LAP5    206

// load adress of a variable
#define     IP_LAV0    209
#define     IP_LAV1    210
#define     IP_LAV2    211
#define     IP_LAV3    212
#define     IP_LAV4    213
#define     IP_LAV5    214

// load adress of a formal argument
#define     IP_LAF0    216
#define     IP_LAF1    217
#define     IP_LAF2    218
#define     IP_LAF3    219
#define     IP_LAF4    220
#define     IP_LAF5    221

#define     IP_THENIF  249 // expecting else after then
#define     IP_MAX1    250 // second opcode for maximum (used internally)
#define     IP_MIN1    251 // second opcode for mimunum (used internally)
#define     IP_RET     253 // return from user function
#define     IP_STOP    254 // end of the equation

#endif
