module mcedef

!     defines variables holding data for opcodes
!     opskip gives number of bytes that follow each opcode in code
!     see file mpeini for more details and initialization
!     see file mscalc for meaning

integer, parameter :: MAXOPC = 255
integer, dimension(0:maxopc), save :: opskip, opdat
character(len = 6), dimension(0:maxopc), save :: opname

!     symbolic names for delimiters
!     symbolic names for operator codes
!     and their internal values

!  OVERVIEW:

!     E_OPAR    0     opening parenthesis
!     E_LIRL    1     load immediate real
!     E_LISUM   2     load immediate sumvariable
!     E_LIRL8   3     load immediate real*8

!     E_LIP0/5   9 .. 14 load immediate parameter
!     E_LIV0/5  17 .. 22  load immediate variable
!     E_LIF0/5  25 .. 30  load immediate formal argument of user function

!     E_NOT     65  .not.
!     E_NEG     73  unary minus
!     E_LOG     74  natural logarithm
!     E_EXP     75  exp function
!     E_ABS     76  absolute value
!     E_SIN     77  sine
!     E_COS     78  cosine
!     E_ATAN    79  arc tangent
!     E_SQRT    80  square root
!     E_NINT    81  round to integer
!     E_LOG10   82  logarithm base 10
!     E_TAN     83  tangent
!     E_ASIN    84  arcsine
!     E_ACOS    85  arccosine
!     E_SINH    86  hyperbolic sine
!     E_COSH    87  hyperbolic cosine
!     E_TANH    88  hyperbolic tangent
!     E_TORL    89  convert logical to real (true ==> 1 and false ==> 0)
!     E_ISVL    90  isvalid (test if number is not NA) THIS OPCODE IS NO LONGER USED
!=    E_ABSZ    91  special absolute value (for Javelin models).
!     E_CUMNOR  92  cumnor function
!     E_INVNOR  93  invnor function
!     E_HYPOT   110 hypot function
!     E_HYPOT1  111 second opcode for hypot function (used internally)
!     E_FIBUR   112 fibur function
!     E_FIBUR1  113 second opcode for fibur function (used internally)
!     E_ADD     129 addition
!     E_SUB     130 subtraction
!     E_MUL     131 multiplication
!     E_DIV     132 division
!     E_POW     133 exponentiation
!     E_MAX     134 maximum
!     E_MIN     135 minimum
!     E_LT      137 .lt.
!     E_GT      138 .gt.
!     E_EQ      139 .eq.
!     E_LE      140 .le.
!     E_GE      141 .ge.
!     E_NE      142 .ne.
!     E_OR      145 .or.
!     E_AND     146 .and.
!=    E_ADDZ    147    addition        NA converted to 0
!=    E_SUBZ    148    subtraction     NA converted to 0
!=    E_MULZ    149    multiplication  NA converted to 1
!=    E_DIVZ    150    division        NA converted to 1
!=    E_LTZ     151    .lt.            false if operand NA
!=    E_GTZ     152    .gt.            false if operand NA
!=    E_EQZ     153    .eq.            false if operand NA
!=    E_LEZ     154    .le.            false if operand NA
!=    E_GEZ     155    .ge.            false if operand NA
!=    E_NEZ     156    .ne.            false if operand NA
!     E_IF      193    if
!     E_ELSE    194    else
!     E_ENDIF   195  endif
!     E_SUM     196 start sum
!     E_ENDSUM  197 end sum
!     E_DEL     198 start del
!     E_ENDDEL  199 end del
!     E_CALL    200 call user statement function
!     E_ULCALL  207 call user language function

!     E_LAP0/5  201 .. 206  load adress of a parameter
!     E_LAV0/5  209 .. 214  load adress of a variable
!     E_LAF0/5  216 .. 221  load adress of a formal argument user function

!     E_THENIF  249 expecting else after then
!     E_MAX1    250  second opcode for maximum (used internally)
!     E_MIN1    251  second opcode for mimunum (used internally)
!     E_RET     253  return from user function
!     E_STOP    254  end of the equation

integer, parameter :: E_OPAR = 0, E_LIRL = 1, E_LISUM = 2, E_LIRL8 = 3

!     Load model parameters/variables/arguments
!     Load Immediate or Load Address
!       P parameter
!       V variable
!       F formal argument of function
!     Suffix 0 .. 5 (!! MUST be in this order)
!                   (!! shift in code is always absolute)
!       0 no loopvar no shift
!       1 loopvar
!       2 shift < 0
!       3 loopvar shift < 0
!       4 shift > 0
!       5 loopvar shift > 0
!     opcodes must be sequential for 0 .. 5 suffix

integer, parameter ::  E_LIP0 = 9, E_LIP1 = E_LIP0 + 1, E_LIP2 = E_LIP0 + 2, &
&                      E_LIP3 = E_LIP0 + 3, E_LIP4 = E_LIP0 + 4, &
&                      E_LIP5 = E_LIP0 + 5
integer, parameter ::  E_LIV0 =17, E_LIV1 = E_LIV0 + 1, E_LIV2 = E_LIV0 + 2, &
&                      E_LIV3 = E_LIV0 + 3, E_LIV4 = E_LIV0 + 4, &
&                      E_LIV5 = E_LIV0 + 5
integer, parameter ::  E_LIF0 =25, E_LIF1 = E_LIF0 + 1, E_LIF2 = E_LIF0 + 2, &
&                      E_LIF3 = E_LIF0 + 3, E_LIF4 = E_LIF0 + 4, &
&                      E_LIF5 = E_LIF0 + 5

! 1-argument functions:
integer, parameter ::  E_NOT  = 65
integer, parameter ::  E_NEG = 73, E_LOG = 74, E_EXP = 75, E_ABS = 76, &
&                      E_SIN = 77, E_COS = 78, E_ATAN= 79, E_SQRT= 80, E_NINT= 81
integer, parameter ::  E_LOG10 = 82, E_TAN  = 83, E_ASIN = 84, E_ACOS = 85, &
&                      E_SINH  = 86, E_COSH = 87, E_TANH = 88, E_TORL = 89
integer, parameter ::  E_ISVL = 90, E_ABSZ = 91
integer, parameter ::  E_CUMNOR = 92, E_INVNOR = 93

! 2-argument functions:
integer, parameter ::  E_HYPOT  = 110, E_HYPOT1 = 111
integer, parameter ::  E_FIBUR  = 112, E_FIBUR1 = 113

! binary operators:
integer, parameter ::  E_ADD  = 129, E_SUB = 130, E_MUL = 131, E_DIV = 132, E_POW  = 133

! max/ min functions:
integer, parameter ::  E_MAX  = 134, E_MIN  = 135

! relational operators:
integer, parameter ::  E_LT   = 137, E_GT = 138, E_EQ  = 139, E_LE = 140, &
&                      E_GE   = 141, E_NE = 142
integer, parameter ::  E_OR = 145, E_AND = 146

! special binary operators:
integer, parameter ::  E_ADDZ = 147, E_SUBZ= 148, E_MULZ= 149, E_DIVZ=150

! special relational operators:
integer, parameter ::  E_LTZ  = 151, E_GTZ= 152, E_EQZ = 153, E_LEZ=154 , &
&                      E_GEZ  = 155, E_NEZ= 156

integer, parameter ::  E_IF  = 193, E_ELSE = 194, E_ENDIF = 195
integer, parameter ::  E_SUM = 196, E_ENDSUM = 197
integer, parameter ::  E_DEL = 198, E_ENDDEL = 199
integer, parameter ::  E_CALL = 200, E_ULCALL = 207

integer, parameter ::  E_LAP0 =201,E_LAP1 = E_LAP0 + 1, E_LAP2 = E_LAP0 + 2, &
&                      E_LAP3 = E_LAP0 + 3, E_LAP4 = E_LAP0 + 4, &
&                      E_LAP5 = E_LAP0 + 5
integer, parameter ::  E_LAV0 =209,E_LAV1 = E_LAV0 + 1, E_LAV2 = E_LAV0 + 2, &
&                      E_LAV3 = E_LAV0 + 3, E_LAV4 = E_LAV0 + 4, &
&                      E_LAV5 = E_LAV0 + 5
integer, parameter ::  E_LAF0 =216,E_LAF1 = E_LAF0 + 1, E_LAF2 = E_LAF0 + 2, &
&                      E_LAF3 = E_LAF0 + 3, E_LAF4 = E_LAF0 + 4, &
&                      E_LAF5 = E_LAF0 + 5

integer, parameter ::  E_THENIF = 249
integer, parameter ::  E_MAX1 = 250, E_MIN1 = 251
integer, parameter ::  E_STOP = 254, E_RET = 253

contains

subroutine mceini

integer ::  last, i

!     contains definitions for setting and getting
!     various data/flags for model code opcodes
!     this file must be included as the last since it contains
!     FTN statement functions
!     the files mcedef and mcedat must be included before this file
!     integer array opdat defined in file mpedef
!     default integer size is at least 32 bits
!     bits counted from right to left (least significant bit is bit 0)
!     incoming precedence in first  8 bits 0 .. 7
!     instack  precedence in second 8 bits 8 .. 15
!     push value flag     in bit 16
!     not allowed in code in bit 17
!     push value flag is used when processing a call by reference
!     function argument
!     PRECIS must be a power of 2
integer ::   precis
integer ::   pvflag
integer ::   rinval
integer ::   hbits
parameter( PRECIS = 256 , HBITS = PRECIS * PRECIS )
parameter( PVFLAG = 1  , RINVAL = 1)
!     statement functions for extracting data from opdat
!     set operator data
!       argument k incoming precedence (0..255)
!       argument l instack  precedence (0..255)
!       argument m 1 to indicate opcode is equivalent to push value
!                  use PVFLAG parameter
!       argument n 1 to indicate opcode should not occur in resulting code
!                  use RINVAL parameter
integer ::  sopdat, icprec,isprec,ioprli,irinvl
integer ::  k,l,m,n
sopdat(k,l,m,n) = k + PRECIS * l + HBITS * ( m + 2 * n )
!     icprec(k)  = opdat(k) & 0xFF
!     isprec(k)  = opdat(k) & 0xFF00
!     ioprli(k)  = opdat(k) & 0x10000  (= (opdat(k) >> 16) & 1)
!     irinvl(k)  = opdat(k) & 0x100000 (= (opdat(k) >> 16) & 2)
icprec(k) = mod( mod(opdat(k),HBITS) , PRECIS)
isprec(k) =      mod(opdat(k),HBITS) / PRECIS
ioprli(k) = mod( opdat(k)/HBITS, 2)
irinvl(k) =    ( opdat(k)/HBITS) / 2

opskip =  0

do  i=0,maxopc
   opname(i) = ' '
enddo

!     opskip gives the number of entries in code that follow the opcode

opskip(E_SUM)    = 2
opskip(E_DEL)    = 1
opskip(E_CALL)   = 1
opskip(E_ULCALL) = 2
opskip(E_LIRL)   = 1
opskip(E_LIRL8)  = 2

do  i = 1, 2
   opskip(E_LIP0 + i - 1) = 1
   opskip(E_LIV0 + i - 1) = 1
   opskip(E_LIF0 + i - 1) = 1
   opskip(E_LAP0 + i - 1) = 1
   opskip(E_LAV0 + i - 1) = 1
   opskip(E_LAF0 + i - 1) = 1
enddo

do  i = 3, 6
   opskip(E_LIP0 + i - 1) = 2
   opskip(E_LIV0 + i - 1) = 2
   opskip(E_LIF0 + i - 1) = 2
   opskip(E_LAP0 + i - 1) = 2
   opskip(E_LAV0 + i - 1) = 2
   opskip(E_LAF0 + i - 1) = 2
enddo

!     setup opname array (contains names op operators/functions)
!     used in printing equations and for error messages
!     not more than 6 chars

!     opname(E_NOT)    = '^'
opname(E_NOT)    = '.not.'

opname(E_NEG)    = '-'
opname(E_LOG)    = 'log'
opname(E_EXP)    = 'exp'
opname(E_ABS)    = 'abs'
opname(E_SIN)    = 'sin'
opname(E_COS)    = 'cos'
opname(E_ATAN)   = 'atan'
opname(E_SQRT)   = 'sqrt'
opname(E_NINT)   = 'nint'
opname(E_LOG10)  = 'log10'
opname(E_TAN)    = 'tan'
opname(E_ASIN)   = 'asin'
opname(E_ACOS)   = 'acos'
opname(E_SINH)   = 'sinh'
opname(E_COSH)   = 'cosh'
opname(E_TANH)   = 'tanh'
opname(E_TORL)   = 'toreal'
opname(E_ISVL)   = 'valid'
opname(E_ADD)    = '+'
opname(E_SUB)    = '-'
opname(E_MUL)    = '*'
opname(E_DIV)    = '/'
opname(E_POW)    = '**'
opname(E_MAX)    = 'max'
opname(E_MIN)    = 'min'
opname(E_HYPOT)  = 'hypot'
opname(E_FIBUR)  = 'fibur'
opname(E_CUMNOR) = 'cumnor'
opname(E_INVNOR) = 'invnor'

opname(E_ADDZ)   = '+'
opname(E_SUBZ)   = '-'
opname(E_MULZ)   = '*'
opname(E_DIVZ)   = '/'

opname(E_ABSZ)   = 'abs'

!     opname(E_LT)     = '<'
!     opname(E_GT)     = '>'
!     opname(E_EQ)     = '='
!     opname(E_LE)     = '<='
!     opname(E_GE)     = '>='
!     opname(E_NE)     = '^='
!     opname(E_OR)     = '|'
!     opname(E_AND)    = '&'

opname(E_LT)     = '.lt.'
opname(E_GT)     = '.gt.'
opname(E_EQ)     = '.eq.'
opname(E_LE)     = '.le.'
opname(E_GE)     = '.ge.'
opname(E_NE)     = '.ne.'
opname(E_OR)     = '.or.'
opname(E_AND)    = '.and.'

opname(E_LTZ)    = '.lt.'
opname(E_GTZ)    = '.gt.'
opname(E_EQZ)    = '.eq.'
opname(E_LEZ)    = '.le.'
opname(E_GEZ)    = '.ge.'
opname(E_NEZ)    = '.ne.'

!     opname(E_IF)     = 'if'
!     opname(E_ELSE)   = 'else'
!     opname(E_ENDIF)  = ' '
!     opname(E_SUM)    = 'sum'
!     opname(E_ENDSUM) = ' '
!     opname(E_DEL)    = 'del'
!     opname(E_ENDDEL) = ' '

!     array opdat contains information on operators
!     needed during print of equation code
!     incoming/instack precedence
!     a flag indicating result type of stack operation (value or address)
!     a flag indicating whether or not this opcode may occur intermediate code

do  i = 0,255
   opdat(i) = sopdat(0,0,PVFLAG,RINVAL)
enddo

!     boolean .or. .and. .not.

opdat(E_OR ) = sopdat(1,1,PVFLAG,0)
opdat(E_AND) = sopdat(2,2,PVFLAG,0)
opdat(E_NOT) = sopdat(3,3,PVFLAG,0)

!     relational

opdat(E_LT ) = sopdat(4,4,PVFLAG,0)
opdat(E_GT ) = sopdat(4,4,PVFLAG,0)
opdat(E_EQ ) = sopdat(4,4,PVFLAG,0)
opdat(E_LE ) = sopdat(4,4,PVFLAG,0)
opdat(E_GE ) = sopdat(4,4,PVFLAG,0)
opdat(E_NE ) = sopdat(4,4,PVFLAG,0)
opdat(E_LTZ) = sopdat(4,4,PVFLAG,0)
opdat(E_GTZ) = sopdat(4,4,PVFLAG,0)
opdat(E_EQZ) = sopdat(4,4,PVFLAG,0)
opdat(E_LEZ) = sopdat(4,4,PVFLAG,0)
opdat(E_GEZ) = sopdat(4,4,PVFLAG,0)
opdat(E_NEZ) = sopdat(4,4,PVFLAG,0)

!     addition substraction

opdat(E_ADD) = sopdat(5,5,PVFLAG,0)
opdat(E_SUB) = sopdat(5,5,PVFLAG,0)
opdat(E_ADDZ)= sopdat(5,5,PVFLAG,0)
opdat(E_SUBZ)= sopdat(5,5,PVFLAG,0)

!     multiplication division

opdat(E_MUL) = sopdat(6,6,PVFLAG,0)
opdat(E_DIV) = sopdat(6,6,PVFLAG,0)
opdat(E_MULZ)= sopdat(6,6,PVFLAG,0)
opdat(E_DIVZ)= sopdat(6,6,PVFLAG,0)

!     exponentiation (right associative) unary minus

opdat(E_POW) = sopdat(8,7,PVFLAG,0)
opdat(E_NEG) = sopdat(9,7,PVFLAG,0)

!     builtin 1 argument functions

opdat(E_LOG) = sopdat(9,9,PVFLAG,0)
opdat(E_EXP) = sopdat(9,9,PVFLAG,0)
opdat(E_ABS) = sopdat(9,9,PVFLAG,0)
opdat(E_ABSZ)= sopdat(9,9,PVFLAG,0)
opdat(E_SIN) = sopdat(9,9,PVFLAG,0)
opdat(E_COS) = sopdat(9,9,PVFLAG,0)
opdat(E_ATAN)= sopdat(9,9,PVFLAG,0)
opdat(E_SQRT)= sopdat(9,9,PVFLAG,0)
opdat(E_NINT)= sopdat(9,9,PVFLAG,0)
opdat(E_LOG10)= sopdat(9,9,PVFLAG,0)
opdat(E_TAN)  = sopdat(9,9,PVFLAG,0)
opdat(E_ASIN) = sopdat(9,9,PVFLAG,0)
opdat(E_ACOS) = sopdat(9,9,PVFLAG,0)
opdat(E_SINH) = sopdat(9,9,PVFLAG,0)
opdat(E_COSH) = sopdat(9,9,PVFLAG,0)
opdat(E_TANH) = sopdat(9,9,PVFLAG,0)
opdat(E_TORL) = sopdat(9,9,PVFLAG,0)
opdat(E_CUMNOR) = sopdat(9,9,PVFLAG,0)
opdat(E_INVNOR) = sopdat(9,9,PVFLAG,0)

opdat(E_ISVL) = sopdat(9,9,PVFLAG,0)

!     builtin functions with  2 arguments

opdat(E_HYPOT) = sopdat(9,9,PVFLAG,0)
opdat(E_FIBUR) = sopdat(9,9,PVFLAG,0)

!     builtin functions with at least 2 arguments

opdat(E_MAX) = sopdat(9,9,PVFLAG,0)
opdat(E_MIN) = sopdat(9,9,PVFLAG,0)

!     IF expression, SUM DEL functions, User functions

opdat(E_IF  )  = sopdat(9,9,PVFLAG,0)
opdat(E_ELSE)  = sopdat(0,0,PVFLAG,0)
opdat(E_ENDIF) = sopdat(0,0,PVFLAG, 0)
opdat(E_THENIF)= sopdat(0,0,PVFLAG, RINVAL)

opdat(E_SUM )  = sopdat(9,9,PVFLAG,0)
opdat(E_ENDSUM)= sopdat(0,0,PVFLAG,0)

opdat(E_DEL )  = sopdat(9,9,PVFLAG,0)
opdat(E_ENDDEL)= sopdat(0,0,PVFLAG,0)

opdat(E_CALL) = sopdat(9,9,PVFLAG,0)
opdat(E_RET ) = sopdat(0,0,PVFLAG,0)
opdat(E_STOP) = sopdat(0,0,PVFLAG,0)

!     left parenthesis

opdat(E_OPAR) = sopdat(0,0,0,RINVAL)

!     load immediate

opdat(E_LIRL ) = sopdat(0,0,PVFLAG,0)
opdat(E_LIRL8) = sopdat(0,0,PVFLAG,0)
opdat(E_LISUM) = sopdat(0,0,PVFLAG,0)

do  i = 1, 6
   opdat(E_LIP0 + i - 1) = sopdat(0,0,PVFLAG,0)
   opdat(E_LIV0 + i - 1) = sopdat(0,0,PVFLAG,0)
   opdat(E_LIF0 + i - 1) = sopdat(0,0,PVFLAG,0)
enddo

!     load address (for argument by reference in function call)

do  i = 1, 6
   opdat(E_LAP0 + i - 1) = sopdat(0,0,0,0)
   opdat(E_LAV0 + i - 1) = sopdat(0,0,0,0)
   opdat(E_LAF0 + i - 1) = sopdat(0,0,0,0)
enddo

return
end subroutine mceini


end module mcedef
