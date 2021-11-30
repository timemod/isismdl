/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         mcparse
#define yylex           mclex
#define yyerror         mcerror
#define yydebug         mcdebug
#define yynerrs         mcnerrs

#define yylval          mclval
#define yychar          mcchar

/* Copy the first part of user declarations.  */
#line 7 "lex_yacc/xpctab.y" /* yacc.c:339  */


#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "symbol.h"
#include "mcinit.h"
#include "xpcdef.h"
#include "mchdr.h"
#include "util.h"
#include "ecode.h"
#include "outmdl.h"
#include "isismdl_types.h"
#include "dependencies.h"
#include "mdldef.h"


void    cleanup( int );
int     check_implicit( int , Symbol *);
int     check_lhs( Symbol * );

void    make_formal( Symbol * );

void    make_param( Symbol * );
int     add_pval( real );
void    set_parval(void);

Enodep  do_var( Symbol *, int, Symbol *, int par);
void    do_lag( Enodep);

void    eqn_prologue(void);
void    eqn_epilogue( Symbol * , int, Symbol *, Enodep , int);

int     sum_prologue( Symbol *, int lo, int hi );
Enodep  sum_epilogue( Enodep );

int     del_prologue( int );
Enodep  del_epilogue( Enodep );

int     ufunc_prologue( Symbol *, int is_ulfunc);
void    ufunc_epilogue( Symbol * , Enodep, int is_ulfunc);
void    funccall_prologue   (void);
Enodep  do_funccall   ( Symbol * , Enodep );
Enodep  do_builtin    ( Symbol * , Enodep );

/* fortran subroutine in mcisis.rf7 */
void FNAME(save_parameter)(FINT *, FREAL8 *);


#line 128 "xpctab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "xpctab.h".  */
#ifndef YY_MC_XPCTAB_H_INCLUDED
# define YY_MC_XPCTAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int mcdebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    T_END = 258,
    T_FUNCTION = 259,
    T_UL_FUNCTION = 260,
    T_FRML = 261,
    T_IDENT = 262,
    T_PARAM = 263,
    T_NAME = 264,
    T_UFUNC = 265,
    T_ULFUNC = 266,
    T_IF = 267,
    T_THEN = 268,
    T_ELSE = 269,
    T_ELSEIF = 270,
    T_ENDIF = 271,
    T_SUM = 272,
    T_DEL = 273,
    T_BUILTIN = 274,
    T_NUMBER = 275,
    T_INTNUM = 276,
    T_GE = 277,
    T_GT = 278,
    T_LT = 279,
    T_LE = 280,
    T_EQ = 281,
    T_NE = 282,
    T_NOT = 283,
    T_AND = 284,
    T_OR = 285,
    UNARYMINUS = 286,
    T_POW = 287
  };
#endif
/* Tokens.  */
#define T_END 258
#define T_FUNCTION 259
#define T_UL_FUNCTION 260
#define T_FRML 261
#define T_IDENT 262
#define T_PARAM 263
#define T_NAME 264
#define T_UFUNC 265
#define T_ULFUNC 266
#define T_IF 267
#define T_THEN 268
#define T_ELSE 269
#define T_ELSEIF 270
#define T_ENDIF 271
#define T_SUM 272
#define T_DEL 273
#define T_BUILTIN 274
#define T_NUMBER 275
#define T_INTNUM 276
#define T_GE 277
#define T_GT 278
#define T_LT 279
#define T_LE 280
#define T_EQ 281
#define T_NE 282
#define T_NOT 283
#define T_AND 284
#define T_OR 285
#define UNARYMINUS 286
#define T_POW 287

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 62 "lex_yacc/xpctab.y" /* yacc.c:355  */

        int     ival;
        real    dval;
        Symbol  *sp;
        Enodep  ep;

#line 239 "xpctab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE mclval;

int mcparse (void);

#endif /* !YY_MC_XPCTAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 256 "xpctab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  24
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   462

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  46
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  38
/* YYNRULES -- Number of rules.  */
#define YYNRULES  114
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  220

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   287

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      40,    41,    35,     4,    42,     3,     2,    36,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    43,    39,
       2,    34,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    44,     2,    45,    33,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    37,    38
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   122,   122,   125,   126,   129,   130,   133,   134,   135,
     136,   139,   143,   146,   149,   150,   153,   154,   155,   158,
     159,   161,   163,   167,   168,   169,   170,   171,   172,   173,
     176,   177,   180,   183,   186,   187,   190,   191,   194,   201,
     202,   204,   206,   210,   211,   214,   217,   227,   228,   229,
     232,   232,   236,   236,   240,   241,   250,   251,   253,   254,
     257,   261,   262,   264,   265,   266,   269,   270,   273,   276,
     277,   279,   282,   285,   286,   289,   292,   296,   297,   300,
     301,   302,   303,   304,   307,   308,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   328,   329,   336,
     337,   343,   344,   353,   354,   355,   356,   357,   360,   361,
     362,   363,   366,   367,   368
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "'-'", "'+'", "\"end\"", "\"function\"",
  "\"ul function\"", "\"frml\"", "\"ident\"", "\"param\"", "\"name\"",
  "\"user function\"", "\"user language function\"", "\"if\"", "\"then\"",
  "\"else\"", "\"elseif\"", "\"endif\"", "\"sum\"", "\"del\"",
  "\"builtin function\"", "\"number\"", "\"integer\"", "\">=\"", "\">\"",
  "\"<\"", "\"<=\"", "\".eq.\"", "\"^=\"", "\"^ or .not.\"",
  "\"& or .and.\"", "\"| or .or.\"", "'^'", "'='", "'*'", "'/'",
  "UNARYMINUS", "T_POW", "';'", "'('", "')'", "','", "':'", "'['", "']'",
  "$accept", "model", "opt_end", "stmtlist", "stmt", "eqtyp", "eqname",
  "keywd", "plist", "pnlist", "pname", "nlist", "snumber", "sinteger",
  "lhsexpl", "lhsimpl", "funcname", "fstmt", "$@1", "ulfstmt", "$@2",
  "formals", "sexpr", "expr", "ifexpr", "ifpart1", "ifthen", "elseif",
  "elseifl", "delarg", "sumarg", "rexpr", "lexpr", "fargs", "ufargs",
  "ufarg", "var", "opt_sign", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,    45,    43,   258,   259,   260,   261,   262,
     263,   264,   265,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,   278,   279,   280,   281,   282,
     283,   284,   285,    94,    61,    42,    47,   286,   287,    59,
      40,    41,    44,    58,    91,    93
};
# endif

#define YYPACT_NINF -107

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-107)))

#define YYTABLE_NINF -113

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     136,   -29,    69,    69,  -107,  -107,     6,  -107,    47,    92,
    -107,   151,  -107,  -107,  -107,  -107,  -107,  -107,  -107,  -107,
    -107,    -7,  -107,    36,  -107,   -12,  -107,  -107,    16,    39,
      44,  -107,  -107,  -107,  -107,  -107,  -107,  -107,    43,   298,
      52,    54,    57,    56,    74,  -107,  -107,  -107,  -107,   118,
    -107,  -107,   154,  -107,   113,  -107,  -107,  -107,  -107,    60,
      84,   229,   229,   112,   112,  -107,  -107,  -107,   117,   128,
     132,   139,   229,   229,   229,   229,   -21,   114,   147,   229,
     150,   165,   168,  -107,  -107,   229,   229,   229,  -107,    17,
     176,   207,   195,  -107,  -107,  -107,   250,  -107,   173,   194,
    -107,  -107,  -107,  -107,   267,   304,   188,   188,    25,    27,
     229,   229,   322,   219,   237,   229,   424,   424,   157,   229,
     229,   229,   229,   229,   229,   229,   229,   229,   229,   229,
     229,   229,   229,  -107,  -107,   229,   229,  -107,   195,  -107,
     227,   228,  -107,  -107,  -107,    31,   222,   241,    12,   220,
     196,   373,   203,  -107,   205,   214,   229,   232,   224,  -107,
     225,   373,   216,  -107,    41,    41,    81,    81,    81,    81,
      81,    81,   424,   409,    81,   188,   188,   188,   373,   358,
    -107,   229,  -107,   249,   257,  -107,  -107,   260,   264,  -107,
    -107,  -107,   229,  -107,   373,    36,   229,   229,  -107,   229,
     229,   373,   256,   259,   245,   271,  -107,   231,   175,   193,
     373,   373,  -107,  -107,  -107,  -107,    36,  -107,  -107,  -107
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,    16,    17,     0,    14,     0,     0,
       5,     0,    15,    47,    48,    49,    50,     7,    52,     8,
      33,     0,    30,   112,     1,     0,     2,     6,    19,    21,
      22,    26,    29,    27,    28,    24,    25,    23,     0,     0,
      20,     0,     0,     0,     0,     9,    31,   113,   114,    32,
      34,    37,     0,     4,     0,    39,    40,    41,    42,     0,
       0,     0,     0,     0,     0,    35,    36,    38,     0,     0,
       0,     0,     0,     0,     0,     0,   103,     0,     0,     0,
       0,     0,     0,    56,    57,     0,     0,     0,    66,     0,
      63,     0,    69,    65,    85,    67,     0,    54,     0,     0,
      43,    44,    45,    46,     0,     0,    83,    84,   112,   112,
       0,     0,     0,     0,     0,     0,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    10,    64,     0,     0,    73,    70,    11,
       0,     0,    53,    12,    13,     0,     0,     0,     0,     0,
      66,     0,     0,    99,    67,     0,     0,     0,     0,    75,
       0,    97,     0,    77,    79,    78,    87,    86,    88,    89,
      91,    92,    93,    94,    90,    80,    81,    82,    68,     0,
      74,     0,    55,     0,     0,   109,   108,     0,     0,   105,
     104,    59,     0,    60,    71,   112,     0,     0,    58,     0,
       0,    51,     0,     0,     0,     0,   100,     0,     0,     0,
      98,    72,   110,   111,   106,   107,   112,    62,    61,    76
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -107,  -107,  -107,  -107,   295,  -107,  -107,   -17,  -107,   299,
    -107,  -107,   273,  -106,   284,   285,   324,  -107,  -107,  -107,
    -107,   270,  -105,   -61,  -107,  -107,  -107,   206,  -107,  -107,
    -107,  -107,  -107,  -107,   230,   153,  -102,   -16
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     8,    26,     9,    10,    11,    39,    40,    21,    22,
      23,    49,    50,    51,    41,    42,    16,    17,    43,    19,
      44,    98,    88,   151,    90,    91,    92,   137,   138,   160,
     158,    93,    94,   162,   152,   153,    95,   147
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      89,    96,   146,   149,    20,   150,   150,    52,   154,   154,
      12,   104,   105,   106,   107,   187,   188,    20,   112,   108,
     119,   120,    58,   109,   116,   117,   118,    53,    47,    48,
      47,    48,    45,    52,   183,   184,   145,    71,   148,    47,
      48,   121,   122,   123,   124,   125,   126,    24,   127,   128,
     -39,   129,   130,   131,   161,   132,   133,   189,   164,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   185,   -40,   178,   179,   130,   131,   -41,   132,
      13,    14,    15,    54,   119,   120,   -42,   150,    61,   207,
     154,    62,    -3,     1,    72,   194,    63,    25,     2,     3,
       4,     5,     6,   -18,   -18,   -18,   -18,   -18,   -18,   -18,
     219,   -18,   -18,   -18,    64,   -18,   130,   131,    73,   132,
     201,    47,    48,    97,    68,    69,    70,    31,    32,    33,
      34,     7,    35,    36,    37,   208,   209,     1,   210,   211,
    -112,  -112,     2,     3,     4,     5,     6,   -18,   -18,   -18,
     -18,   -18,   -18,   -18,   110,   -18,   -18,   -18,   100,   -18,
     119,   120,    28,    29,    30,    31,    32,    33,    34,   101,
      35,    36,    37,   102,    38,     7,    66,    67,   119,   120,
     103,   121,   122,   123,   124,   125,   126,   111,   127,   128,
     113,   129,   130,   131,   134,   132,   119,   120,   163,   121,
     122,   123,   124,   125,   126,   114,   127,   128,   115,   129,
     130,   131,   136,   132,   140,   141,   217,   121,   122,   123,
     124,   125,   126,   135,   127,   128,   132,   129,   130,   131,
     157,   132,    74,    75,   218,   142,   141,  -101,  -101,   182,
      76,    77,    78,    79,   191,   192,  -102,  -102,    80,    81,
      82,    83,    84,   119,   120,   193,   192,   198,   199,    85,
     159,   181,    86,   186,    67,   190,   195,   196,   197,    87,
     119,   120,   202,   216,   121,   122,   123,   124,   125,   126,
     203,   127,   128,   204,   129,   130,   131,   205,   132,   139,
     214,   121,   122,   123,   124,   125,   126,   212,   127,   128,
     213,   129,   130,   131,    27,   132,   143,   119,   120,    55,
      56,    57,    31,    32,    33,    34,   215,    35,    36,    37,
      46,    38,    65,    59,    60,   119,   120,    18,   121,   122,
     123,   124,   125,   126,    99,   127,   128,   156,   129,   130,
     131,   155,   132,   144,   180,   206,   121,   122,   123,   124,
     125,   126,     0,   127,   128,     0,   129,   130,   131,     0,
     132,   119,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   119,   120,     0,     0,
       0,     0,   121,   122,   123,   124,   125,   126,     0,   127,
     128,     0,   129,   130,   131,     0,   132,   121,   122,   123,
     124,   125,   126,     0,   127,   128,     0,   129,   130,   131,
       0,   132,   119,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,   120,     0,
       0,     0,     0,   121,   122,   123,   124,   125,   126,     0,
     127,     0,     0,   129,   130,   131,     0,   132,   121,   122,
     123,   124,   125,   126,     0,     0,     0,     0,   129,   130,
     131,     0,   132
};

static const yytype_int16 yycheck[] =
{
      61,    62,   108,   109,    11,   110,   111,    23,   110,   111,
      39,    72,    73,    74,    75,     3,     4,    11,    79,    40,
       3,     4,    39,    44,    85,    86,    87,    39,     3,     4,
       3,     4,    39,    49,     3,     4,    11,    54,    11,     3,
       4,    24,    25,    26,    27,    28,    29,     0,    31,    32,
      34,    34,    35,    36,   115,    38,    39,    45,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,    41,    34,   135,   136,    35,    36,    34,    38,
      11,    12,    13,    40,     3,     4,    34,   192,    34,   195,
     192,    34,     0,     1,    34,   156,    40,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
     216,    19,    20,    21,    40,    23,    35,    36,    34,    38,
     181,     3,     4,    11,    11,    12,    13,    14,    15,    16,
      17,    39,    19,    20,    21,   196,   197,     1,   199,   200,
      22,    23,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    40,    19,    20,    21,    41,    23,
       3,     4,    11,    12,    13,    14,    15,    16,    17,    41,
      19,    20,    21,    41,    23,    39,    22,    23,     3,     4,
      41,    24,    25,    26,    27,    28,    29,    40,    31,    32,
      40,    34,    35,    36,    18,    38,     3,     4,    41,    24,
      25,    26,    27,    28,    29,    40,    31,    32,    40,    34,
      35,    36,    17,    38,    41,    42,    41,    24,    25,    26,
      27,    28,    29,    16,    31,    32,    38,    34,    35,    36,
      11,    38,     3,     4,    41,    41,    42,    41,    42,    11,
      11,    12,    13,    14,    41,    42,    41,    42,    19,    20,
      21,    22,    23,     3,     4,    41,    42,    41,    42,    30,
      23,    34,    33,    41,    23,    45,    34,    43,    43,    40,
       3,     4,    23,    42,    24,    25,    26,    27,    28,    29,
      23,    31,    32,    23,    34,    35,    36,    23,    38,    39,
      45,    24,    25,    26,    27,    28,    29,    41,    31,    32,
      41,    34,    35,    36,     9,    38,    39,     3,     4,    11,
      12,    13,    14,    15,    16,    17,    45,    19,    20,    21,
      21,    23,    49,    39,    39,     3,     4,     3,    24,    25,
      26,    27,    28,    29,    64,    31,    32,    15,    34,    35,
      36,   111,    38,    39,   138,   192,    24,    25,    26,    27,
      28,    29,    -1,    31,    32,    -1,    34,    35,    36,    -1,
      38,     3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    -1,    -1,     3,     4,    -1,    -1,
      -1,    -1,    24,    25,    26,    27,    28,    29,    -1,    31,
      32,    -1,    34,    35,    36,    -1,    38,    24,    25,    26,
      27,    28,    29,    -1,    31,    32,    -1,    34,    35,    36,
      -1,    38,     3,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,    -1,
      -1,    -1,    -1,    24,    25,    26,    27,    28,    29,    -1,
      31,    -1,    -1,    34,    35,    36,    -1,    38,    24,    25,
      26,    27,    28,    29,    -1,    -1,    -1,    -1,    34,    35,
      36,    -1,    38
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     6,     7,     8,     9,    10,    39,    47,    49,
      50,    51,    39,    11,    12,    13,    62,    63,    62,    65,
      11,    54,    55,    56,     0,     5,    48,    50,    11,    12,
      13,    14,    15,    16,    17,    19,    20,    21,    23,    52,
      53,    60,    61,    64,    66,    39,    55,     3,     4,    57,
      58,    59,    83,    39,    40,    11,    12,    13,    53,    60,
      61,    34,    34,    40,    40,    58,    22,    23,    11,    12,
      13,    53,    34,    34,     3,     4,    11,    12,    13,    14,
      19,    20,    21,    22,    23,    30,    33,    40,    68,    69,
      70,    71,    72,    77,    78,    82,    69,    11,    67,    67,
      41,    41,    41,    41,    69,    69,    69,    69,    40,    44,
      40,    40,    69,    40,    40,    40,    69,    69,    69,     3,
       4,    24,    25,    26,    27,    28,    29,    31,    32,    34,
      35,    36,    38,    39,    18,    16,    17,    73,    74,    39,
      41,    42,    41,    39,    39,    11,    59,    83,    11,    59,
      68,    69,    80,    81,    82,    80,    15,    11,    76,    23,
      75,    69,    79,    41,    69,    69,    69,    69,    69,    69,
      69,    69,    69,    69,    69,    69,    69,    69,    69,    69,
      73,    34,    11,     3,     4,    41,    41,     3,     4,    45,
      45,    41,    42,    41,    69,    34,    43,    43,    41,    42,
      15,    69,    23,    23,    23,    23,    81,    59,    69,    69,
      69,    69,    41,    41,    45,    45,    42,    41,    41,    59
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    46,    47,    48,    48,    49,    49,    50,    50,    50,
      50,    50,    50,    50,    50,    50,    51,    51,    51,    52,
      52,    52,    52,    53,    53,    53,    53,    53,    53,    53,
      54,    54,    55,    56,    57,    57,    58,    58,    59,    60,
      60,    60,    60,    61,    61,    61,    61,    62,    62,    62,
      64,    63,    66,    65,    67,    67,    68,    68,    68,    68,
      68,    68,    68,    68,    68,    68,    69,    69,    70,    71,
      71,    72,    73,    74,    74,    75,    76,    77,    77,    77,
      77,    77,    77,    77,    77,    77,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    79,    79,    80,
      80,    81,    81,    82,    82,    82,    82,    82,    82,    82,
      82,    82,    83,    83,    83
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     0,     2,     1,     2,     2,     2,     3,
       5,     5,     6,     6,     1,     2,     1,     1,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     2,     1,     1,     2,     2,     1,     2,     1,
       1,     1,     1,     4,     4,     4,     4,     1,     1,     1,
       0,     7,     0,     5,     1,     3,     1,     1,     4,     4,
       4,     6,     6,     1,     2,     1,     1,     1,     3,     1,
       2,     4,     4,     1,     2,     1,     5,     3,     3,     3,
       3,     3,     3,     2,     2,     1,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     2,     2,     1,     3,     1,
       3,     1,     1,     1,     4,     4,     6,     6,     4,     4,
       6,     6,     0,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:
#line 126 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {reset_scanner(); YYACCEPT;}
#line 1534 "xpctab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 137 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { eqn_epilogue(NULL, (yyvsp[-4].ival), (yyvsp[-3].sp), (yyvsp[-1].ep), 0);}
#line 1540 "xpctab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 140 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { eqn_epilogue(NULL, (yyvsp[-4].ival), (yyvsp[-3].sp), (yyvsp[-1].ep), 1);}
#line 1546 "xpctab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 144 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { eqn_epilogue((yyvsp[-4].sp), (yyvsp[-5].ival), (yyvsp[-3].sp), (yyvsp[-1].ep), 0);}
#line 1552 "xpctab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 147 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { eqn_epilogue((yyvsp[-4].sp), (yyvsp[-5].ival), (yyvsp[-3].sp), (yyvsp[-1].ep), 1);}
#line 1558 "xpctab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 149 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.sp) = NULL;}
#line 1564 "xpctab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 150 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { cleanup(0); yyerrok; }
#line 1570 "xpctab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 153 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { eqn_prologue(); (yyval.ival) = 1;}
#line 1576 "xpctab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 154 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { eqn_prologue(); (yyval.ival) = 0;}
#line 1582 "xpctab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 155 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { eqn_prologue(); (yyval.ival) = 0;}
#line 1588 "xpctab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 167 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.sp)=(yyvsp[0].sp);}
#line 1594 "xpctab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 168 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.sp)=(yyvsp[0].sp);}
#line 1600 "xpctab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 169 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.sp)=(yyvsp[0].sp);}
#line 1606 "xpctab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 170 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.sp)=(yyvsp[0].sp);}
#line 1612 "xpctab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 171 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.sp)=(yyvsp[0].sp);}
#line 1618 "xpctab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 172 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.sp)=(yyvsp[0].sp);}
#line 1624 "xpctab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 173 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.sp)=(yyvsp[0].sp);}
#line 1630 "xpctab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 180 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { set_parval(); }
#line 1636 "xpctab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 183 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { make_param((yyvsp[0].sp));}
#line 1642 "xpctab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 186 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { if (add_pval((yyvsp[0].dval))) YYERROR;}
#line 1648 "xpctab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 187 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { if (add_pval((yyvsp[0].dval))) YYERROR;}
#line 1654 "xpctab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 190 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.dval) = ((yyvsp[-1].ival) == '-') ? -(yyvsp[0].dval) : (yyvsp[0].dval); }
#line 1660 "xpctab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 191 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.dval) = (yyvsp[0].ival);}
#line 1666 "xpctab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 194 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ival) = ((yyvsp[-1].ival) == '-') ? -(yyvsp[0].ival) : (yyvsp[0].ival); }
#line 1672 "xpctab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 201 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { check_lhs((yyvsp[0].sp)); }
#line 1678 "xpctab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 203 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { check_lhs((yyvsp[0].sp)); }
#line 1684 "xpctab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 205 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { check_lhs((yyvsp[0].sp)); }
#line 1690 "xpctab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 207 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { check_lhs((yyvsp[0].sp)); }
#line 1696 "xpctab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 210 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.sp) = (yyvsp[-1].sp); if(check_implicit((yyvsp[-3].ival), (yyvsp[-1].sp))) YYERROR; }
#line 1702 "xpctab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 213 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.sp) = (yyvsp[-1].sp); if(check_implicit((yyvsp[-3].ival), (yyvsp[-1].sp))) YYERROR; }
#line 1708 "xpctab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 216 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.sp) = (yyvsp[-1].sp); if(check_implicit((yyvsp[-3].ival), (yyvsp[-1].sp))) YYERROR; }
#line 1714 "xpctab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 219 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.sp) = (yyvsp[-1].sp); if(check_implicit((yyvsp[-3].ival), (yyvsp[-1].sp))) YYERROR; }
#line 1720 "xpctab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 232 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { if (ufunc_prologue((yyvsp[0].sp), 0)) YYERROR;}
#line 1726 "xpctab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 233 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { ufunc_epilogue((yyvsp[-6].sp), (yyvsp[0].ep), 0); }
#line 1732 "xpctab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 236 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { if (ufunc_prologue((yyvsp[0].sp), 1)) YYERROR;}
#line 1738 "xpctab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 237 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { ufunc_epilogue((yyvsp[-4].sp), 0, 1); }
#line 1744 "xpctab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 240 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { make_formal((yyvsp[0].sp)); }
#line 1750 "xpctab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 241 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { make_formal((yyvsp[0].sp)); }
#line 1756 "xpctab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 250 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = mk_const((yyvsp[0].dval)); }
#line 1762 "xpctab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 251 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = mk_const((yyvsp[0].ival)); }
#line 1768 "xpctab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 253 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_builtin((yyvsp[-3].sp), (yyvsp[-1].ep)); }
#line 1774 "xpctab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 254 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { 
                              if (((yyval.ep) = do_funccall((yyvsp[-3].sp), (yyvsp[-1].ep))) == 0) YYERROR;
                                           }
#line 1782 "xpctab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 257 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { 
                              if (((yyval.ep) = do_funccall((yyvsp[-3].sp), (yyvsp[-1].ep))) == 0) YYERROR;
                                           }
#line 1790 "xpctab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 261 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = del_epilogue((yyvsp[-1].ep)); }
#line 1796 "xpctab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 262 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = sum_epilogue((yyvsp[-1].ep)); }
#line 1802 "xpctab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 270 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { do_lag((yyvsp[0].ep));}
#line 1808 "xpctab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 273 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.ep) = add_elsepart((yyvsp[-2].ep), (yyvsp[0].ep));}
#line 1814 "xpctab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 276 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.ep) = (yyvsp[0].ep);}
#line 1820 "xpctab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 277 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.ep) = add_elsepart((yyvsp[-1].ep), (yyvsp[0].ep));}
#line 1826 "xpctab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 279 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.ep) = mk_ifnode((yyvsp[-2].ep), (yyvsp[0].ep));}
#line 1832 "xpctab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 282 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.ep) = mk_ifnode((yyvsp[-2].ep), (yyvsp[0].ep));}
#line 1838 "xpctab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 285 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.ep) = (yyvsp[0].ep);}
#line 1844 "xpctab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 286 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    {(yyval.ep) = add_elsepart((yyvsp[-1].ep), (yyvsp[0].ep));}
#line 1850 "xpctab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 289 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { if( del_prologue((yyvsp[0].ival)) ) YYERROR; }
#line 1856 "xpctab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 293 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { if( sum_prologue((yyvsp[-4].sp), (yyvsp[-2].ival), (yyvsp[0].ival)) ) YYERROR; }
#line 1862 "xpctab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 296 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = (yyvsp[-1].ep); }
#line 1868 "xpctab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 297 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { Binary :
                                                  (yyval.ep) = mk_binenode((yyvsp[-1].ival), (yyvsp[-2].ep), (yyvsp[0].ep));
                                        }
#line 1876 "xpctab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 300 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1882 "xpctab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 301 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1888 "xpctab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 302 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1894 "xpctab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 303 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1900 "xpctab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 304 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { Unary :
                                                 (yyval.ep) = mk_unop((yyvsp[-1].ival), (yyvsp[0].ep));
                                        }
#line 1908 "xpctab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 307 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = (yyvsp[0].ep); }
#line 1914 "xpctab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 311 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1920 "xpctab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 312 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1926 "xpctab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 313 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1932 "xpctab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 314 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1938 "xpctab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 315 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1944 "xpctab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 316 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1950 "xpctab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 317 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1956 "xpctab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 318 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1962 "xpctab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 319 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Binary; }
#line 1968 "xpctab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 320 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { goto Unary;  }
#line 1974 "xpctab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 321 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = mk_unop(T_NOT, (yyvsp[0].ep)); }
#line 1980 "xpctab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 328 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = mk_fargs(0 , (yyvsp[0].ep) ); }
#line 1986 "xpctab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 329 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = mk_fargs((yyvsp[-2].ep), (yyvsp[0].ep) ); }
#line 1992 "xpctab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 336 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = mk_fargs(0 , (yyvsp[0].ep) ); }
#line 1998 "xpctab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 337 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = mk_fargs((yyvsp[-2].ep), (yyvsp[0].ep) ); }
#line 2004 "xpctab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 353 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[0].sp),  0, 0 , 0); }
#line 2010 "xpctab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 354 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[-3].sp), (yyvsp[-1].ival), 0 , 0); }
#line 2016 "xpctab.c" /* yacc.c:1646  */
    break;

  case 105:
#line 355 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[-3].sp),  0, (yyvsp[-1].sp), 0); }
#line 2022 "xpctab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 356 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[-5].sp),-(yyvsp[-1].ival), (yyvsp[-3].sp), 0); }
#line 2028 "xpctab.c" /* yacc.c:1646  */
    break;

  case 107:
#line 357 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[-5].sp), (yyvsp[-1].ival), (yyvsp[-3].sp), 0); }
#line 2034 "xpctab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 360 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[-3].sp), (yyvsp[-1].ival), 0 , 1); }
#line 2040 "xpctab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 361 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[-3].sp),  0, (yyvsp[-1].sp), 1); }
#line 2046 "xpctab.c" /* yacc.c:1646  */
    break;

  case 110:
#line 362 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[-5].sp),-(yyvsp[-1].ival), (yyvsp[-3].sp), 1); }
#line 2052 "xpctab.c" /* yacc.c:1646  */
    break;

  case 111:
#line 363 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ep) = do_var((yyvsp[-5].sp), (yyvsp[-1].ival), (yyvsp[-3].sp), 1); }
#line 2058 "xpctab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 366 "lex_yacc/xpctab.y" /* yacc.c:1646  */
    { (yyval.ival) = 0; }
#line 2064 "xpctab.c" /* yacc.c:1646  */
    break;


#line 2068 "xpctab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 374 "lex_yacc/xpctab.y" /* yacc.c:1906  */


/*
 * flags and variables for checking
 *
 */

static  int     insum = 0;      /* 1 when doing Sum(...) */
static  int     indel = 0;      /* 1 when doing Del(...) */
static  int     indef = 0;      /* 1 when doing Function */

static  int     deloffset = 0;

static  int     Curfunc_sum = 0;
static  int     Curfunc_del = 0;

static  char    *Curfunc = NULL;
static  int     in_equation = 0;
static  Symbol  *sumvar  = NULL;

static  char    **argnames = NULL;
Argdef  **argdefs  = NULL;
static  size_t  argcnt = 0;

static int      varCount = 0;  /* count of model variables */
static int      eqCount = 0;   /* count of equations */
static int      parCount = 0;  /* count of model parameters */
static int      funcCount = 0; /* count of user functions  */
static int      ulFuncCount = 0; /* count of user language function */
/* Parameters */

static  size_t  curpcnt = 0;       /* count of numbers in pval             */
static  double  pval[MAXPARAM]; /* temp storage for param values        */
static  Symbol *Curparam;


/*
 * prototypes
 */

static  void new_var( Symbol *, int , int);

/*
 * the following is for "semantic" error checks
 *
 * all error messages and error flags setting
 * are handled through xpcerrmsg(...)
 *
 * all routines and actions taken have been setup
 * in such a way that they are robust wrt errors
 *
 * the Enode generating functions are error resistant
 */

static  int     errflag = 0;
static  void    name_err ( char *, Symbol *);

void    cleanup( int comefrom )
{
    /*
     * comefrom == 0 called by yacc error production
     * else from one of the following functions
     */

    size_t  i;

    if( sumvar )
    {
        efree( sumvar->u.sumvarp );
        sym_uninstall( Stp, sumvar->name );
        sumvar = NULL;
    }

    if( argnames )
    {
        for( i = 0; i < argcnt ; i++ ) {
            sym_uninstall( Stp, argnames[i]);
            /* note: argnames[i] points to the name field of the
             * symbol with name argnames[i]: therefore argnames[i]
             * should not be freed (the memory is already freed
             * in sym_uninstall) */
        }
        argcnt = 0;
        free(argnames);
        argnames = NULL;
    }

    if( errflag )
    {
        if( Curfunc || in_equation )
            reset_enode();
    }

    if( comefrom == 0 )
    {
        if( Curfunc )
            sym_uninstall(Stp, Curfunc);
    }

    if (argdefs) {
       free(argdefs);
    }

    errflag = 0;
    Curfunc = NULL;
    in_equation = 0;
    argdefs = NULL;
    insum   = indel = indef = Curfunc_sum = Curfunc_del = 0;
}

int  ufunc_prologue(Symbol *sp, int is_ulfunc) {

    if (sp->xpctype != XP_UNDEF) {
	/* function name already used for something else */
        name_err(sp->name, sp);
	return 1;
    }
	
    indef    = 1;
    Curfunc  = sp->name;

    if( strlen(Curfunc) > MAX_UFNNLEN )
        mcwarn("Function name <%s> too long (max %d chars)\n",
                Curfunc, MAX_UFNNLEN );
	
    if (is_ulfunc) {
        sp->type    = T_ULFUNC;
        sp->xpctype = XP_ULFUNC;
    } else {
        sp->type    = T_UFUNC;
        sp->xpctype = XP_FUNC;
    }
    sp->u.funp  = NULL;

    if (!is_ulfunc) {
        open_enode();
    }

    return 0;
}

void    ufunc_epilogue( Symbol *sp, Enodep ex, int is_ulfunc)
{
    size_t  i;
    Funcdef *fnp;

    if( errflag )
    {
        sym_uninstall(Stp, Curfunc);        /* delete function */
        cleanup(1);
        sp->u.funp = NULL;
        return;
    }

    sp->u.funp   = fnp = emalloc( sizeof(Funcdef) );
    fnp->flags.is_ulfunc = is_ulfunc;
    fnp->flags.use_sum = Curfunc_sum;
    fnp->flags.use_del = Curfunc_del;

    fnp->argcnt   = argcnt;
    fnp->argnames = emalloc( argcnt * sizeof(char*) );
    fnp->argdp    = argdefs;

    if (is_ulfunc) {
	    fnp->func_index = ulFuncCount++; 
            /* user language function index (first function has index 0) */
    } else {
        fnp->func_index = funcCount++; /* function index (first function has 
                                           index 0) */
    }

    if( argnames )
    {
        for( i = 0; i < argcnt ; i++ ) {

            /* copy argnames to func def
             * and only then uninstall from symbol table
             */
            fnp->argnames[i] = estrdup( argnames[i] );
            sym_uninstall( Stp, argnames[i]);

            /* note: argnames[i] points to the name field of the
             * symbol with name argnames[i]: therefore argnames[i]
             * should not be freed (the memory is already freed
             * in sym_uninstall) */
        }

        free(argnames);
    }

    if (!is_ulfunc) {
        fnp->fcp    = close_enode();    /* save code of func expression */
        fnp->estart = ex;
    } else {
        fnp->fcp    = NULL;
        fnp->estart = -1;
    }

    indef   = 0;
    Curfunc_sum = Curfunc_del = 0;
    Curfunc = NULL;
    argcnt = 0;
    argnames = NULL;
    argdefs = NULL;

    if (options.McIsisMdl) {
        if (!is_ulfunc) {
            if (errcnt == 0 && warncnt == 0) {
	        out_ipcode(sp);
            }
            fnp->fcp = NULL;
        }
     } else {
         new_eqn(sp);
     }
}

Enodep  do_builtin( Symbol *sp, Enodep arglist )
{
    size_t  largcnt = arglist_count(arglist);

    if( XP_builtin2p(sp->xpctype) )
    {
        if( largcnt < 2 )
            xpcerrmsg("Builtin <%s> requires at least 2 arguments",
                        sp->name);
    }
    else if( XP_builtin1(sp->xpctype) )
    {
        if( largcnt != 1 )
            xpcerrmsg("Builtin <%s> requires 1 argument",
                        sp->name);
    }
    else if( XP_builtin2(sp->xpctype) )
    {
        if( largcnt != 2 )
            xpcerrmsg("Builtin <%s> requires 2 arguments",
                        sp->name);
    }
    else
        xpcerrmsg("Unknown builtin <%s>", sp->name);

    return mk_builtin(sp, arglist);
}

Enodep  do_funccall( Symbol *sp, Enodep arglist)
{

    /*
     * Handle function call. Returns an pointer to the enode tree
     * of the function call, or 0 if an error has occurred and parsing
     * of the current statement should be aborted.
     */

    if (indel && sp->u.funp->flags.use_del) {
        xpcerrmsg( "Use of function %s implies a nested del",
                    sp->name);
    }

    if (insum && sp->u.funp->flags.use_sum) {
        xpcerrmsg( "Use of function %s implies a nested sum",
                    sp->name);
    }

    if (Curfunc != NULL && strcmp(sp->name, Curfunc) == 0) {
        xpcerrmsg("Function %s calls itself. Recursion is not allowed",
                  sp->name);
	/* stop parsing current equation or function */
        return 0;
    }

    /* check number of arguments passed and argument types.
     * do not stop parsing the current equation or function in case of an error */
    check_fcall(sp, arglist, sumvar, deloffset);

    return mk_funccall(sp, arglist);
}

void eqn_prologue (void)
{
    open_enode();
    in_equation = 1;
}

void  eqn_epilogue(Symbol *eqnamesp, int isfrml, Symbol *lhs, Enodep ex,
                       int isimplicit) {
    Equation *eqnp;
    Symbol *eqsp;
    char *eqn_name, *p;

    if (eqnamesp != NULL) {
        eqn_name = eqnamesp->name;
        if ((eqnamesp->type == T_BUILTIN) || (eqnamesp->xpctype == XP_CMD)) {
           /* the name of the equation is a keyword: convert to uppercase */
           for (p = eqn_name; (*p = tolower(*p)) != '\0'; p++);
        }
    } else 
        eqn_name = lhs->name;

    eqn_name = (eqnamesp != NULL) ?  eqnamesp->name : lhs->name;
	
    eqsp = sym_create(Eqntp, eqn_name, T_NAME); 

    /* remove unused name from symol table. */
    if (eqnamesp != NULL && eqnamesp->xpctype == XP_UNDEF) {
        sym_uninstall(Stp, eqn_name);
    }

    if( eqsp->xpctype != XP_UNDEF )
        xpcerrmsg( "Duplicate equation %s", eqsp->name);


    eqsp->xpctype = XP_EQN;

    if (errflag) {
        cleanup(1);
	eqsp->u.eqnp = NULL;
        return;
    }

    eqsp->u.eqnp = eqnp = emalloc( sizeof(Equation) );

    eqnp->eqtype = 0;
    eqnp->eq_index = eqCount++; /* equation index (first equation has index 0) */
    if( isfrml ) {
        Set_Frmleqn(eqnp);
    } 
    if( isimplicit ) {
        Set_Impleqn(eqnp);
    }

    lhs->u.varp->vtype = isfrml ? ENDO_FRML : ENDO_IDENT;

    eqnp->lhs    = lhs;
    eqnp->ecp    = close_enode();
    eqnp->estart = ex;

    in_equation = 0;

    if (options.McIsisMdl) {
        if (errcnt == 0 && warncnt == 0) {
            out_ipcode(eqsp);
        }
        eqnp->ecp    = NULL;
    } else {
        new_eqn(eqsp);
    }

    if (options.gen_dep) {
        eqnp->deps = close_dependencies();
    } else {
        eqnp->deps = NULL;
    }
}

int     sum_prologue( Symbol *sp, int lo, int hi )
{
    /*
     * create sum variable
     */

    SumVariable *vp;

    if( insum )
    {
        xpcerrmsg( "Nested sum not allowed\n" );
        return 1;
    }

    if( indef )
        Curfunc_sum = 1;

    insum = 1;

    if( sp->xpctype != XP_UNDEF )
    {
        /*
         * already exists as something else
         * create new purely as local variable
         */

        sumvar = sym_install( Stp, sp->name, T_NAME);
    }
    else
        sumvar = sp;

    /*
     * set type to sum variable
     */

    sumvar->xpctype = XP_SVAR;

    sumvar->u.sumvarp  = vp = emalloc( sizeof(SumVariable) );

    if( lo > hi )
        xpcerrmsg( "Invalid sum index range\n" );

    vp->low  = lo;
    vp->high = hi;

    return 0;
}

Enodep  sum_epilogue( Enodep sumexpr )
{
    /*
     * Symbol structure for a sum index may not
     * be deleted but must be unlinked
     * this is to remember the name of the sum index
     */

    Enodep rval;
    Symbol  *sp;

    if (sumvar) {
        sp = sym_unlink( Stp, sumvar->name );
        sumvar = NULL;
        rval = mk_sum( sp, sumexpr);
        /*
         * NOTE: memory leak in this code. sumvar is not any more in the
         *       symbol table, so the associated memory is not released
         *       in function free_symtab (see init.c). However, when the 
         *       compiler is used to compile Isis code, the memory is 
         *       released in outipcode.c. 
         */
    } else {
        rval = 0;
    }

    insum = 0;
    return rval;
}

int     del_prologue( int delarg )
{
    if( indel )
    {
        xpcerrmsg( "Nested del not allowed\n" );
        return 1;
    }

    if( indef )
        Curfunc_del = 1;

    indel = 1;

    if( delarg == 0 )
        xpcerrmsg( "Zero del not possible\n" );

    deloffset = delarg;

    return 0;
}

Enodep  del_epilogue( Enodep delexpr)
{
    Enodep  rval = mk_del(deloffset, delexpr);

    indel = deloffset = 0;
    return rval;
}

int check_implicit( int d , Symbol *sp )
{
    if( d != 0 ) {
        xpcerrmsg( "Implicit equation must be 0(name)\n" );
        return 1;
    }

    return check_lhs(sp);
}

int  check_lhs( Symbol *sp )
{
    /*
     * check if lhs variable is already endogenous
     * if it is then it has already been used on lhs
     * for the time being use xpctype field
     */

    if (sp->type == T_BUILTIN || sp->xpctype == XP_CMD) {
        xpcerrmsg( "error: %s is a reserved word", sp->name);
        return 1;
    } else if( sp->xpctype == XP_ENDO ) {
        xpcerrmsg( "Variable %s used more than once in left hand side",
                            sp->name);
        return 1;
    } else if( sp->xpctype != XP_UNDEF && sp->xpctype != XP_EXO ) {
        /* the name is aleady used for something else */
        name_err(sp->name, sp);
        return 1;
    } else if( sp->xpctype == XP_UNDEF )
        /* for the time being assume that variable has type ENDO_IDENT,
         * the correct type will be set in eqn_epilogue */
        new_var( sp, XP_ENDO, ENDO_IDENT);
    else
    {
        sp->xpctype       = XP_ENDO;
    }
    return 0;
}

void    make_formal( Symbol *sp )
{
    /*
     * create a new formal argument for a function
     */

    Argdef  *argp;

    if( sp->xpctype == XP_FNARG )
        xpcerrmsg( "duplicate argument %s in function %s\n",
                            sp->name, Curfunc);
    else if( sp->xpctype == XP_UNDEF )      /* totally new name */
        sp->xpctype = XP_FNARG;
    else
    {   /* make a new copy */

        sp = sym_install( Stp, sp->name, T_NAME );
        sp->xpctype = XP_FNARG;
    }

    sp->u.handle = argcnt; /* offset base 0 */

    argnames = erealloc( argnames, (argcnt + 1) * sizeof( char   * ) );
    argdefs  = erealloc( argdefs , (argcnt + 1) * sizeof( Argdef * ) );

    argp = argdefs[argcnt] = emalloc( sizeof(Argdef) );
    argp->atype   = ARG_NOTUSE;
    argp->lower      = INT_MAX;
    if (!options.McIsisMdl) {
        argp->lower_del  = INT_MAX;
    }
    argp->upper      = INT_MIN;

    argnames[argcnt++] = sp->name;
}

void do_lag(Enodep ep)
/*
 * check lags applied to variables that occur in an expr.
 * calculate max lead/lag/dellag implied by lag specifier
 * adjust max lead and lag of variables or function argument
 * keep track of argument type (by reference if explicit lag)
 */
{
    check_lag(ep, argdefs, sumvar, deloffset);
}

static  void new_var( Symbol *sp , int xpctype, int vtype )
{
    Variable *vp;

    if( strlen(sp->name) > MAX_VARNLEN )
        mcwarn("Variable name <%s> too long (max %d chars)\n",
                    sp->name , MAX_VARNLEN );

    sp->u.varp  = vp = emalloc( sizeof(Variable) );
    sp->xpctype = xpctype;

    vp->vtype   = vtype;
    vp->var_index = varCount++; /* variable index (first variable has index 0) */
    if (!options.McIsisMdl) {
        vp->maxlead = 0;
        vp->maxlag  = 0;
    }
}

static  void check_var( Symbol *sp )
{

    /*
     * create a variable
     * may be sum variable or local
     *
     */

    if( sp->xpctype != XP_UNDEF && sp->xpctype != XP_SVAR &&
        sp->xpctype != XP_FNARG && sp->xpctype != XP_EXO  &&
        sp->xpctype != XP_ENDO  && sp->xpctype != XP_PARAM
      ) {
        name_err(sp->name, sp);
    } else if( sp->xpctype == XP_UNDEF )
        /* for the time being assume that variable is exogeneous */
        new_var(sp, XP_EXO, EXO);

}

Enodep  do_var( Symbol *sp, int lagoffset , Symbol *sumsp, int parentheses)
{
    int     lagtype = 0;

    check_var(sp);

    if( sumsp )
    {
        if( !insum ) {
            if (parentheses) {
                xpcerrmsg( "Unknown function %s", sp->name);
            } else {
               xpcerrmsg( "Invalid variable lag %s. Variable lags may only be used in sum expression", sumsp->name);
            }
            return 0;
        } else if( sumsp->xpctype != XP_SVAR ) {
            if (parentheses) {
                xpcerrmsg( "Unknown function %s", sp->name);
            } else {
                xpcerrmsg( "Invalid variable lag %s. Only a sum index may be used as variable lag", sumsp->name);
            }
            return 0;
        }
    }

    if( (lagoffset || sumsp != NULL) && sp->xpctype == XP_SVAR )
        xpcerrmsg("Sum index cannot be lagged\n");

    if (sumsp)
        lagtype = 2;
    else if (lagoffset != 0)
        lagtype = 1;
    else
        lagtype = 0;

    return mk_var( sp, lagtype, lagoffset);
}

static  void name_err( char *name, Symbol *sp)
{
    char * typename;

    switch (sp->xpctype) {
    case XP_PARAM:
       typename = "parameter";
       break;
    case XP_FUNC:
       typename = "user function";
       break;
    case XP_ENDO:
       typename = "endogenous variable";
       break;
    case XP_EXO:
       typename = "exogenous variable";
       break;
    default:
      typename = NULL;
      break;
    }
    
    if (typename != NULL)
        xpcerrmsg("Name %s already used as %s name", name, typename);
    else   
        xpcerrmsg("Name %s already used for something else", name);
}

void    xpcerrmsg( char *fmt, ... )
{
    va_list ap;

    va_start(ap, fmt);
        mcerrmessage(fmt, ap);
    va_end(ap);
    ++errflag;
}

void make_param(Symbol *sp)
{
    /*
     * create a parameter variable
     */

    curpcnt = 0;
    Curparam = sp;

    /*
     * for non-strict compilation (parameters may be used
     * before they have been defined), the model has already
     * been scanned for parameters. All parameters are already 
     * in the symbol table
     */
    if (!options.Strict) {
       return;
    }

    /* the following code is for the strict compilation */

    char *pname = sp->name;

    if (sp->xpctype == XP_PARAM )
    {
        mcerror( "Duplicate parameter name %s", sp->name);
    }
    else if (sp->xpctype != XP_UNDEF)
    {
        /* parameter name used for something else */
        name_err(sp->name, sp);
    }

    if( strlen(pname) > MAX_PARNLEN ) {
        mcerror("Parameter name %s too long (max %d chars)",
                    pname, MAX_PARNLEN);
    }

    sp->xpctype = XP_PARAM;
    sp->u.parp = NULL;
}

int add_pval(double rval) {
    /* adds a value to the current parameter. returns 0 if successful,
     * or 1 if the number of values > MAXPARAM */
    if( curpcnt >= MAXPARAM ) {
        mcerror( "Too many parameter values for parameter %s. The maximum is %d.",
                  Curparam->name, MAXPARAM);
        return 1;
    }

    pval[curpcnt++] = rval;
    return 0;
}

void set_parval()
{
    /*
     * create new parameter
     * save parameter values
     */

     Param *p;

     if (options.Strict) {
         /* create a Param, and set its length.
          * (for non-strict compilation this has already
          * been done while scanning parameters */
         Curparam->u.parp = p = emalloc( sizeof(Param) );
         p->cnt = curpcnt;
         p->par_index = parCount++; /* variable index (first variable has index 0) */
     }

    if (options.McIsisMdl) {
        FNAME(save_parameter)((FINT *) &curpcnt, pval);
    } else {
        p = Curparam->u.parp;
        if (curpcnt == 1 ) {
            p->u.dval = pval[0];
        } else {
            p->u.dp = emalloc( sizeof(real) * curpcnt );
            memcpy( p->u.dp, pval, sizeof(real) * curpcnt );
        }
        new_par(Curparam);
     }

#if 0
        fprintf( stderr, "Parameter name %s # of values %u\n",
                        Curparam->name , curpcnt);
        if( curpcnt == 1 )
                fprintf(stderr, "%10.4f\n", p->u.dval );
        else
        {
                size_t i;
                for( i = 0; i < curpcnt ; i++ )
                {
                        fprintf(stderr, "%10.4f", p->u.dp[i] );
                        if( (i + 1) % 8 == 0 )
                                fprintf( stderr, "\n" );
                }
                fprintf(stderr, "\n" );
        }
#endif
}

void mcparse_init(void) 
    /* initialise variables for parser */
{
   eqCount = 0;
   varCount = 0;
   parCount = 0;
   funcCount = 0;
   ulFuncCount = 0;
}
