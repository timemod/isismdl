/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
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
#define YYBISON_VERSION "2.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         mcparse
#define yylex           mclex
#define yyerror         mcerror
#define yylval          mclval
#define yychar          mcchar
#define yydebug         mcdebug
#define yynerrs         mcnerrs


/* Copy the first part of user declarations.  */

/* Line 268 of yacc.c  */
#line 7 "xpctab.y"


#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "symbol.h"
#ifndef MCISIS
#include "mdldef.h"
#endif
#include "mcinit.h"
#include "xpcdef.h"
#include "mchdr.h"
#include "util.h"
#include "ecode.h"
#include "outmdl.h"
#include "macromodtypes.h"
#include "dependencies.h"


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

static  void    eqtyp_err(Symbol *);

#ifdef MCISIS
/* fortran subroutine in mcisis.rf7 */
void FNAME(save_parameter)(FINT *, FREAL8 *);
#endif



/* Line 268 of yacc.c  */
#line 138 "xpctab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
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
     NE = 282,
     T_NOT = 283,
     T_AND = 284,
     T_OR = 285,
     T_NE = 286,
     UNARYMINUS = 287,
     T_POW = 288
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 293 of yacc.c  */
#line 66 "xpctab.y"

        int     ival;
        real    dval;
        Symbol  *sp;
        Enodep  ep;



/* Line 293 of yacc.c  */
#line 216 "xpctab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 343 of yacc.c  */
#line 228 "xpctab.c"

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
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
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
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  36
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   417

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  47
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  39
/* YYNRULES -- Number of rules.  */
#define YYNRULES  117
/* YYNRULES -- Number of states.  */
#define YYNSTATES  223

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   288

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      41,    42,    36,     4,    43,     3,     2,    37,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    44,    40,
       2,    34,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    45,     2,    46,    33,     2,     2,     2,     2,     2,
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
      27,    28,    29,    30,    31,    32,    35,    38,    39
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,     7,    10,    12,    15,    18,    21,
      25,    31,    37,    44,    51,    53,    56,    58,    60,    62,
      64,    66,    68,    70,    72,    74,    76,    78,    80,    82,
      84,    86,    88,    90,    92,    94,    97,   100,   102,   104,
     107,   110,   112,   115,   117,   119,   121,   123,   128,   133,
     138,   140,   142,   144,   145,   153,   154,   160,   162,   166,
     168,   170,   175,   180,   185,   192,   199,   201,   204,   206,
     208,   210,   214,   216,   219,   224,   229,   231,   234,   236,
     242,   246,   250,   254,   258,   262,   266,   269,   272,   274,
     278,   282,   286,   290,   294,   298,   302,   306,   310,   313,
     316,   318,   322,   324,   328,   330,   332,   334,   339,   344,
     351,   358,   363,   368,   375,   382,   383,   385
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      48,     0,    -1,    50,    49,    -1,    -1,     5,    40,    -1,
      51,    -1,    50,    51,    -1,     6,    65,    -1,     7,    67,
      -1,    10,    56,    40,    -1,    52,    62,    34,    71,    40,
      -1,    52,    63,    34,    71,    40,    -1,    52,    54,    62,
      34,    71,    40,    -1,    52,    54,    63,    34,    71,    40,
      -1,    40,    -1,     1,    40,    -1,     8,    -1,     9,    -1,
      53,    -1,    11,    -1,    55,    -1,    12,    -1,    13,    -1,
      11,    -1,    55,    -1,    12,    -1,    13,    -1,    21,    -1,
      19,    -1,    20,    -1,    14,    -1,    16,    -1,    17,    -1,
      15,    -1,    57,    -1,    56,    57,    -1,    58,    59,    -1,
      11,    -1,    60,    -1,    59,    60,    -1,    85,    22,    -1,
      61,    -1,    85,    23,    -1,    11,    -1,    12,    -1,    13,
      -1,    55,    -1,    23,    41,    11,    42,    -1,    23,    41,
      12,    42,    -1,    23,    41,    13,    42,    -1,    11,    -1,
      12,    -1,    13,    -1,    -1,    64,    66,    41,    69,    42,
      34,    71,    -1,    -1,    64,    68,    41,    69,    42,    -1,
      11,    -1,    69,    43,    11,    -1,    22,    -1,    23,    -1,
      21,    41,    81,    42,    -1,    12,    41,    82,    42,    -1,
      13,    41,    82,    42,    -1,    20,    41,    77,    44,    71,
      42,    -1,    19,    41,    78,    44,    71,    42,    -1,    72,
      -1,    72,    18,    -1,    79,    -1,    70,    -1,    84,    -1,
      73,    16,    71,    -1,    74,    -1,    74,    76,    -1,    14,
      71,    15,    71,    -1,    17,    71,    15,    71,    -1,    75,
      -1,    76,    75,    -1,    23,    -1,    11,    34,    61,    43,
      61,    -1,    41,    71,    42,    -1,    71,     4,    71,    -1,
      71,     3,    71,    -1,    71,    36,    71,    -1,    71,    37,
      71,    -1,    71,    39,    71,    -1,     3,    71,    -1,     4,
      71,    -1,    80,    -1,    71,    25,    71,    -1,    71,    24,
      71,    -1,    71,    26,    71,    -1,    71,    27,    71,    -1,
      71,    34,    71,    -1,    71,    28,    71,    -1,    71,    35,
      71,    -1,    71,    31,    71,    -1,    71,    32,    71,    -1,
      30,    71,    -1,    33,    71,    -1,    71,    -1,    81,    43,
      71,    -1,    83,    -1,    82,    43,    83,    -1,    70,    -1,
      84,    -1,    11,    -1,    11,    45,    61,    46,    -1,    11,
      45,    11,    46,    -1,    11,    45,    11,     3,    23,    46,
      -1,    11,    45,    11,     4,    23,    46,    -1,    11,    41,
      61,    42,    -1,    11,    41,    11,    42,    -1,    11,    41,
      11,     3,    23,    42,    -1,    11,    41,    11,     4,    23,
      42,    -1,    -1,     3,    -1,     4,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   127,   127,   130,   131,   134,   135,   138,   139,   140,
     142,   145,   149,   152,   155,   156,   159,   160,   161,   165,
     166,   167,   168,   171,   172,   174,   176,   180,   181,   182,
     183,   184,   185,   186,   189,   190,   193,   196,   199,   200,
     203,   204,   207,   214,   215,   217,   219,   223,   224,   227,
     237,   238,   239,   242,   242,   246,   246,   250,   251,   260,
     261,   263,   264,   267,   271,   272,   274,   275,   276,   279,
     280,   283,   286,   287,   289,   292,   295,   296,   299,   302,
     306,   307,   310,   311,   312,   313,   314,   317,   318,   321,
     322,   323,   324,   325,   326,   327,   328,   329,   330,   331,
     338,   339,   346,   347,   353,   354,   363,   364,   365,   366,
     367,   370,   371,   372,   373,   376,   377,   378
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
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
  "\"& or .and.\"", "\"| or .or.\"", "'^'", "'='", "T_NE", "'*'", "'/'",
  "UNARYMINUS", "T_POW", "';'", "'('", "')'", "','", "':'", "'['", "']'",
  "$accept", "model", "opt_end", "stmtlist", "stmt", "eqtyp",
  "eqtyp_error", "eqname", "keywd", "plist", "pnlist", "pname", "nlist",
  "snumber", "sinteger", "lhsexpl", "lhsimpl", "funcname", "fstmt", "$@1",
  "ulfstmt", "$@2", "formals", "sexpr", "expr", "ifexpr", "ifpart1",
  "ifthen", "elseif", "elseifl", "delarg", "sumarg", "rexpr", "lexpr",
  "fargs", "ufargs", "ufarg", "var", "opt_sign", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,    45,    43,   258,   259,   260,   261,   262,
     263,   264,   265,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,   278,   279,   280,   281,   282,
     283,   284,   285,    94,    61,   286,    42,    47,   287,   288,
      59,    40,    41,    44,    58,    91,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    47,    48,    49,    49,    50,    50,    51,    51,    51,
      51,    51,    51,    51,    51,    51,    52,    52,    52,    53,
      53,    53,    53,    54,    54,    54,    54,    55,    55,    55,
      55,    55,    55,    55,    56,    56,    57,    58,    59,    59,
      60,    60,    61,    62,    62,    62,    62,    63,    63,    63,
      64,    64,    64,    66,    65,    68,    67,    69,    69,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    70,    71,
      71,    72,    73,    73,    74,    75,    76,    76,    77,    78,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    80,
      80,    80,    80,    80,    80,    80,    80,    80,    80,    80,
      81,    81,    82,    82,    83,    83,    84,    84,    84,    84,
      84,    84,    84,    84,    84,    85,    85,    85
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     0,     2,     1,     2,     2,     2,     3,
       5,     5,     6,     6,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     1,     2,
       2,     1,     2,     1,     1,     1,     1,     4,     4,     4,
       1,     1,     1,     0,     7,     0,     5,     1,     3,     1,
       1,     4,     4,     4,     6,     6,     1,     2,     1,     1,
       1,     3,     1,     2,     4,     4,     1,     2,     1,     5,
       3,     3,     3,     3,     3,     3,     2,     2,     1,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     2,     2,
       1,     3,     1,     3,     1,     1,     1,     4,     4,     6,
       6,     4,     4,     6,     6,     0,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,    16,    17,     0,    19,    21,    22,
      30,    33,    31,    32,    28,    29,    27,    14,     0,     0,
       5,     0,    18,    20,    15,    50,    51,    52,    53,     7,
      55,     8,    37,     0,    34,   115,     1,     0,     2,     6,
      23,    25,    26,     0,     0,    24,     0,     0,     0,     0,
       9,    35,   116,   117,    36,    38,    41,     0,     4,     0,
      43,    44,    45,    46,     0,     0,     0,     0,     0,     0,
      39,    40,    42,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,    59,    60,     0,
       0,     0,    69,     0,    66,     0,    72,    68,    88,    70,
       0,    57,     0,     0,    47,    48,    49,     0,     0,    86,
      87,   115,   115,     0,     0,     0,     0,     0,     0,    98,
      99,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    10,    67,     0,     0,
      76,    73,    11,     0,     0,    56,    12,    13,     0,     0,
       0,     0,     0,    69,     0,     0,   102,    70,     0,     0,
       0,     0,    78,     0,   100,     0,    80,    82,    81,    90,
      89,    91,    92,    94,    96,    97,    93,    95,    83,    84,
      85,    71,     0,    77,     0,    58,     0,     0,   112,   111,
       0,     0,   108,   107,    62,     0,    63,    74,   115,     0,
       0,    61,     0,     0,    54,     0,     0,     0,     0,   103,
       0,     0,     0,   101,    75,   113,   114,   109,   110,   115,
      65,    64,    79
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    18,    38,    19,    20,    21,    22,    44,    23,    33,
      34,    35,    54,    55,    56,    46,    47,    28,    29,    48,
      31,    49,   102,    92,   154,    94,    95,    96,   140,   141,
     163,   161,    97,    98,   165,   155,   156,    99,   150
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -110
static const yytype_int16 yypact[] =
{
     134,   -36,   160,   160,  -110,  -110,     4,  -110,  -110,  -110,
    -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,    26,    34,
    -110,   108,  -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,
    -110,  -110,  -110,    -4,  -110,    27,  -110,   -22,  -110,  -110,
      47,    50,    52,    54,   147,    71,    92,    98,    89,   111,
    -110,  -110,  -110,  -110,    79,  -110,  -110,    94,  -110,   204,
    -110,  -110,  -110,  -110,   131,   135,   209,   209,    77,    77,
    -110,  -110,  -110,   144,   149,   163,   209,   209,   209,   209,
     -25,   169,   194,   209,   195,   196,   197,  -110,  -110,   209,
     209,   209,  -110,   221,   222,   225,   234,  -110,  -110,  -110,
     240,  -110,    96,   140,  -110,  -110,  -110,   259,   278,   215,
     215,    18,    74,   209,   209,   304,   248,   246,   209,   378,
     378,    72,   209,   209,   209,   209,   209,   209,   209,   209,
     209,   209,   209,   209,   209,   209,  -110,  -110,   209,   209,
    -110,   234,  -110,   236,   262,  -110,  -110,  -110,    29,   247,
     255,    24,   242,   151,   341,   159,  -110,   176,   184,   209,
     258,   253,  -110,   256,   341,   191,  -110,    55,    55,    76,
      76,    76,    76,    76,   378,   359,    76,    76,   215,   215,
     215,   341,   322,  -110,   209,  -110,   288,   293,  -110,  -110,
     297,   298,  -110,  -110,  -110,   209,  -110,   341,    27,   209,
     209,  -110,   209,   209,   341,   280,   281,   287,   296,  -110,
     284,   153,   172,   341,   341,  -110,  -110,  -110,  -110,    27,
    -110,  -110,  -110
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -110,  -110,  -110,  -110,   282,  -110,  -110,  -110,    -7,  -110,
     291,  -110,  -110,   301,  -109,   290,   307,   349,  -110,  -110,
    -110,  -110,   295,  -108,   -66,  -110,  -110,  -110,   219,  -110,
    -110,  -110,  -110,  -110,  -110,   257,   175,  -105,   -16
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -116
static const yytype_int16 yytable[] =
{
      93,   100,   149,   152,    24,   153,   153,    32,   157,   157,
     107,   108,   109,   110,    45,    32,   111,   115,    58,    57,
     112,    52,    53,   119,   120,   121,    36,   190,   191,   148,
      52,    53,   186,   187,    -3,     1,    50,    63,    57,    37,
       2,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,   164,    14,    15,    16,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     192,   188,   181,   182,    17,   122,   123,    52,    53,   122,
     123,   -43,    52,    53,   -44,   151,   -45,   153,   101,   210,
     157,   133,   134,   197,   135,    59,   124,   125,   126,   127,
     128,  -115,  -115,   129,   130,   -46,   131,   132,   133,   134,
     222,   135,   133,   134,   166,   135,    71,    72,   204,    40,
      41,    42,    10,    11,    12,    13,    66,    14,    15,    16,
      68,    43,    67,   211,   212,     1,   213,   214,   143,   144,
       2,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    69,    14,    15,    16,   122,   123,    60,    61,
      62,    10,    11,    12,    13,    76,    14,    15,    16,    77,
      43,    25,    26,    27,    17,   122,   123,   124,   125,   126,
     127,   128,   145,   144,   129,   130,   104,   131,   132,   133,
     134,   105,   135,  -104,  -104,   220,   124,   125,   126,   127,
     128,   194,   195,   129,   130,   106,   131,   132,   133,   134,
     113,   135,    78,    79,   221,    73,    74,    75,  -105,  -105,
      80,    81,    82,    83,   122,   123,   196,   195,    84,    85,
      86,    87,    88,   201,   202,   114,   116,   117,   118,    89,
     137,   138,    90,   122,   123,   124,   125,   126,   127,   128,
      91,   139,   129,   130,   135,   131,   132,   133,   134,   160,
     135,   136,   122,   123,   124,   125,   126,   127,   128,   162,
     184,   129,   130,   185,   131,   132,   133,   134,    72,   135,
     142,   122,   123,   124,   125,   126,   127,   128,   193,   189,
     129,   130,   198,   131,   132,   133,   134,   199,   135,   146,
     200,    39,   124,   125,   126,   127,   128,   122,   123,   129,
     130,   205,   131,   132,   133,   134,   206,   135,   147,   159,
     207,   208,   215,   216,    51,   122,   123,   219,   124,   125,
     126,   127,   128,   217,    64,   129,   130,   203,   131,   132,
     133,   134,   218,   135,   122,   123,   124,   125,   126,   127,
     128,    65,    30,   129,   130,    70,   131,   132,   133,   134,
     183,   135,   122,   123,   103,   124,   125,   126,   127,   128,
     209,   158,   129,   130,     0,   131,   132,   133,   134,     0,
     135,   122,   123,   124,   125,   126,   127,   128,     0,     0,
     129,     0,     0,   131,   132,   133,   134,     0,   135,     0,
       0,     0,   124,   125,   126,   127,   128,     0,     0,     0,
       0,     0,   131,   132,   133,   134,     0,   135
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-110))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
      66,    67,   111,   112,    40,   113,   114,    11,   113,   114,
      76,    77,    78,    79,    21,    11,    41,    83,    40,    35,
      45,     3,     4,    89,    90,    91,     0,     3,     4,    11,
       3,     4,     3,     4,     0,     1,    40,    44,    54,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,   118,    19,    20,    21,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
      46,    42,   138,   139,    40,     3,     4,     3,     4,     3,
       4,    34,     3,     4,    34,    11,    34,   195,    11,   198,
     195,    36,    37,   159,    39,    41,    24,    25,    26,    27,
      28,    22,    23,    31,    32,    34,    34,    35,    36,    37,
     219,    39,    36,    37,    42,    39,    22,    23,   184,    11,
      12,    13,    14,    15,    16,    17,    34,    19,    20,    21,
      41,    23,    34,   199,   200,     1,   202,   203,    42,    43,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    41,    19,    20,    21,     3,     4,    11,    12,
      13,    14,    15,    16,    17,    34,    19,    20,    21,    34,
      23,    11,    12,    13,    40,     3,     4,    24,    25,    26,
      27,    28,    42,    43,    31,    32,    42,    34,    35,    36,
      37,    42,    39,    42,    43,    42,    24,    25,    26,    27,
      28,    42,    43,    31,    32,    42,    34,    35,    36,    37,
      41,    39,     3,     4,    42,    11,    12,    13,    42,    43,
      11,    12,    13,    14,     3,     4,    42,    43,    19,    20,
      21,    22,    23,    42,    43,    41,    41,    41,    41,    30,
      18,    16,    33,     3,     4,    24,    25,    26,    27,    28,
      41,    17,    31,    32,    39,    34,    35,    36,    37,    11,
      39,    40,     3,     4,    24,    25,    26,    27,    28,    23,
      34,    31,    32,    11,    34,    35,    36,    37,    23,    39,
      40,     3,     4,    24,    25,    26,    27,    28,    46,    42,
      31,    32,    34,    34,    35,    36,    37,    44,    39,    40,
      44,    19,    24,    25,    26,    27,    28,     3,     4,    31,
      32,    23,    34,    35,    36,    37,    23,    39,    40,    15,
      23,    23,    42,    42,    33,     3,     4,    43,    24,    25,
      26,    27,    28,    46,    44,    31,    32,    15,    34,    35,
      36,    37,    46,    39,     3,     4,    24,    25,    26,    27,
      28,    44,     3,    31,    32,    54,    34,    35,    36,    37,
     141,    39,     3,     4,    69,    24,    25,    26,    27,    28,
     195,   114,    31,    32,    -1,    34,    35,    36,    37,    -1,
      39,     3,     4,    24,    25,    26,    27,    28,    -1,    -1,
      31,    -1,    -1,    34,    35,    36,    37,    -1,    39,    -1,
      -1,    -1,    24,    25,    26,    27,    28,    -1,    -1,    -1,
      -1,    -1,    34,    35,    36,    37,    -1,    39
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    19,    20,    21,    40,    48,    50,
      51,    52,    53,    55,    40,    11,    12,    13,    64,    65,
      64,    67,    11,    56,    57,    58,     0,     5,    49,    51,
      11,    12,    13,    23,    54,    55,    62,    63,    66,    68,
      40,    57,     3,     4,    59,    60,    61,    85,    40,    41,
      11,    12,    13,    55,    62,    63,    34,    34,    41,    41,
      60,    22,    23,    11,    12,    13,    34,    34,     3,     4,
      11,    12,    13,    14,    19,    20,    21,    22,    23,    30,
      33,    41,    70,    71,    72,    73,    74,    79,    80,    84,
      71,    11,    69,    69,    42,    42,    42,    71,    71,    71,
      71,    41,    45,    41,    41,    71,    41,    41,    41,    71,
      71,    71,     3,     4,    24,    25,    26,    27,    28,    31,
      32,    34,    35,    36,    37,    39,    40,    18,    16,    17,
      75,    76,    40,    42,    43,    42,    40,    40,    11,    61,
      85,    11,    61,    70,    71,    82,    83,    84,    82,    15,
      11,    78,    23,    77,    71,    81,    42,    71,    71,    71,
      71,    71,    71,    71,    71,    71,    71,    71,    71,    71,
      71,    71,    71,    75,    34,    11,     3,     4,    42,    42,
       3,     4,    46,    46,    42,    43,    42,    71,    34,    44,
      44,    42,    43,    15,    71,    23,    23,    23,    23,    83,
      61,    71,    71,    71,    71,    42,    42,    46,    46,    43,
      42,    42,    61
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* This macro is provided for backward compatibility. */

#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

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
#ifndef	YYINITDEPTH
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
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
  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  YYSIZE_T yysize1;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = 0;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
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
                yysize1 = yysize + yytnamerr (0, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
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

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

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

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
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
  int yytoken;
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

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

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
      yychar = YYLEX;
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
  *++yyvsp = yylval;

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
     `$$ = $1'.

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

/* Line 1806 of yacc.c  */
#line 131 "xpctab.y"
    {reset_scanner(); YYACCEPT;}
    break;

  case 10:

/* Line 1806 of yacc.c  */
#line 143 "xpctab.y"
    { eqn_epilogue(NULL, (yyvsp[(1) - (5)].ival), (yyvsp[(2) - (5)].sp), (yyvsp[(4) - (5)].ep), 0);}
    break;

  case 11:

/* Line 1806 of yacc.c  */
#line 146 "xpctab.y"
    { eqn_epilogue(NULL, (yyvsp[(1) - (5)].ival), (yyvsp[(2) - (5)].sp), (yyvsp[(4) - (5)].ep), 1);}
    break;

  case 12:

/* Line 1806 of yacc.c  */
#line 150 "xpctab.y"
    { eqn_epilogue((yyvsp[(2) - (6)].sp), (yyvsp[(1) - (6)].ival), (yyvsp[(3) - (6)].sp), (yyvsp[(5) - (6)].ep), 0);}
    break;

  case 13:

/* Line 1806 of yacc.c  */
#line 153 "xpctab.y"
    { eqn_epilogue((yyvsp[(2) - (6)].sp), (yyvsp[(1) - (6)].ival), (yyvsp[(3) - (6)].sp), (yyvsp[(5) - (6)].ep), 1);}
    break;

  case 14:

/* Line 1806 of yacc.c  */
#line 155 "xpctab.y"
    { (yyval.sp) = NULL;}
    break;

  case 15:

/* Line 1806 of yacc.c  */
#line 156 "xpctab.y"
    { cleanup(0); yyerrok; }
    break;

  case 16:

/* Line 1806 of yacc.c  */
#line 159 "xpctab.y"
    { eqn_prologue(); (yyval.ival) = 1;}
    break;

  case 17:

/* Line 1806 of yacc.c  */
#line 160 "xpctab.y"
    { eqn_prologue(); (yyval.ival) = 0;}
    break;

  case 18:

/* Line 1806 of yacc.c  */
#line 161 "xpctab.y"
    { /* error: illegal equation type */
                          eqtyp_err((yyvsp[(1) - (1)].sp)); (yyval.ival) = 0;}
    break;

  case 19:

/* Line 1806 of yacc.c  */
#line 165 "xpctab.y"
    {(yyval.sp) = (yyvsp[(1) - (1)].sp);}
    break;

  case 20:

/* Line 1806 of yacc.c  */
#line 166 "xpctab.y"
    {(yyval.sp) = (yyvsp[(1) - (1)].sp);}
    break;

  case 21:

/* Line 1806 of yacc.c  */
#line 167 "xpctab.y"
    {(yyval.sp) = (yyvsp[(1) - (1)].sp);}
    break;

  case 22:

/* Line 1806 of yacc.c  */
#line 168 "xpctab.y"
    {(yyval.sp) = (yyvsp[(1) - (1)].sp);}
    break;

  case 27:

/* Line 1806 of yacc.c  */
#line 180 "xpctab.y"
    {(yyval.sp)=(yyvsp[(1) - (1)].sp);}
    break;

  case 28:

/* Line 1806 of yacc.c  */
#line 181 "xpctab.y"
    {(yyval.sp)=(yyvsp[(1) - (1)].sp);}
    break;

  case 29:

/* Line 1806 of yacc.c  */
#line 182 "xpctab.y"
    {(yyval.sp)=(yyvsp[(1) - (1)].sp);}
    break;

  case 30:

/* Line 1806 of yacc.c  */
#line 183 "xpctab.y"
    {(yyval.sp)=(yyvsp[(1) - (1)].sp);}
    break;

  case 31:

/* Line 1806 of yacc.c  */
#line 184 "xpctab.y"
    {(yyval.sp)=(yyvsp[(1) - (1)].sp);}
    break;

  case 32:

/* Line 1806 of yacc.c  */
#line 185 "xpctab.y"
    {(yyval.sp)=(yyvsp[(1) - (1)].sp);}
    break;

  case 33:

/* Line 1806 of yacc.c  */
#line 186 "xpctab.y"
    {(yyval.sp)=(yyvsp[(1) - (1)].sp);}
    break;

  case 36:

/* Line 1806 of yacc.c  */
#line 193 "xpctab.y"
    { set_parval(); }
    break;

  case 37:

/* Line 1806 of yacc.c  */
#line 196 "xpctab.y"
    { make_param((yyvsp[(1) - (1)].sp));}
    break;

  case 38:

/* Line 1806 of yacc.c  */
#line 199 "xpctab.y"
    { if (add_pval((yyvsp[(1) - (1)].dval))) YYERROR;}
    break;

  case 39:

/* Line 1806 of yacc.c  */
#line 200 "xpctab.y"
    { if (add_pval((yyvsp[(2) - (2)].dval))) YYERROR;}
    break;

  case 40:

/* Line 1806 of yacc.c  */
#line 203 "xpctab.y"
    { (yyval.dval) = ((yyvsp[(1) - (2)].ival) == '-') ? -(yyvsp[(2) - (2)].dval) : (yyvsp[(2) - (2)].dval); }
    break;

  case 41:

/* Line 1806 of yacc.c  */
#line 204 "xpctab.y"
    {(yyval.dval) = (yyvsp[(1) - (1)].ival);}
    break;

  case 42:

/* Line 1806 of yacc.c  */
#line 207 "xpctab.y"
    { (yyval.ival) = ((yyvsp[(1) - (2)].ival) == '-') ? -(yyvsp[(2) - (2)].ival) : (yyvsp[(2) - (2)].ival); }
    break;

  case 43:

/* Line 1806 of yacc.c  */
#line 214 "xpctab.y"
    { check_lhs((yyvsp[(1) - (1)].sp)); }
    break;

  case 44:

/* Line 1806 of yacc.c  */
#line 216 "xpctab.y"
    { check_lhs((yyvsp[(1) - (1)].sp)); }
    break;

  case 45:

/* Line 1806 of yacc.c  */
#line 218 "xpctab.y"
    { check_lhs((yyvsp[(1) - (1)].sp)); }
    break;

  case 46:

/* Line 1806 of yacc.c  */
#line 220 "xpctab.y"
    { check_lhs((yyvsp[(1) - (1)].sp)); }
    break;

  case 47:

/* Line 1806 of yacc.c  */
#line 223 "xpctab.y"
    { (yyval.sp) = (yyvsp[(3) - (4)].sp); if(check_implicit((yyvsp[(1) - (4)].ival), (yyvsp[(3) - (4)].sp))) YYERROR; }
    break;

  case 48:

/* Line 1806 of yacc.c  */
#line 226 "xpctab.y"
    { (yyval.sp) = (yyvsp[(3) - (4)].sp); if(check_implicit((yyvsp[(1) - (4)].ival), (yyvsp[(3) - (4)].sp))) YYERROR; }
    break;

  case 49:

/* Line 1806 of yacc.c  */
#line 229 "xpctab.y"
    { (yyval.sp) = (yyvsp[(3) - (4)].sp); if(check_implicit((yyvsp[(1) - (4)].ival), (yyvsp[(3) - (4)].sp))) YYERROR; }
    break;

  case 53:

/* Line 1806 of yacc.c  */
#line 242 "xpctab.y"
    { if (ufunc_prologue((yyvsp[(1) - (1)].sp), 0)) YYERROR;}
    break;

  case 54:

/* Line 1806 of yacc.c  */
#line 243 "xpctab.y"
    { ufunc_epilogue((yyvsp[(1) - (7)].sp), (yyvsp[(7) - (7)].ep), 0); }
    break;

  case 55:

/* Line 1806 of yacc.c  */
#line 246 "xpctab.y"
    { if (ufunc_prologue((yyvsp[(1) - (1)].sp), 1)) YYERROR;}
    break;

  case 56:

/* Line 1806 of yacc.c  */
#line 247 "xpctab.y"
    { ufunc_epilogue((yyvsp[(1) - (5)].sp), 0, 1); }
    break;

  case 57:

/* Line 1806 of yacc.c  */
#line 250 "xpctab.y"
    { make_formal((yyvsp[(1) - (1)].sp)); }
    break;

  case 58:

/* Line 1806 of yacc.c  */
#line 251 "xpctab.y"
    { make_formal((yyvsp[(3) - (3)].sp)); }
    break;

  case 59:

/* Line 1806 of yacc.c  */
#line 260 "xpctab.y"
    { (yyval.ep) = mk_const((yyvsp[(1) - (1)].dval)); }
    break;

  case 60:

/* Line 1806 of yacc.c  */
#line 261 "xpctab.y"
    { (yyval.ep) = mk_const((yyvsp[(1) - (1)].ival)); }
    break;

  case 61:

/* Line 1806 of yacc.c  */
#line 263 "xpctab.y"
    { (yyval.ep) = do_builtin((yyvsp[(1) - (4)].sp), (yyvsp[(3) - (4)].ep)); }
    break;

  case 62:

/* Line 1806 of yacc.c  */
#line 264 "xpctab.y"
    { 
                              if (((yyval.ep) = do_funccall((yyvsp[(1) - (4)].sp), (yyvsp[(3) - (4)].ep))) == 0) YYERROR;
                                           }
    break;

  case 63:

/* Line 1806 of yacc.c  */
#line 267 "xpctab.y"
    { 
                              if (((yyval.ep) = do_funccall((yyvsp[(1) - (4)].sp), (yyvsp[(3) - (4)].ep))) == 0) YYERROR;
                                           }
    break;

  case 64:

/* Line 1806 of yacc.c  */
#line 271 "xpctab.y"
    { (yyval.ep) = del_epilogue((yyvsp[(5) - (6)].ep)); }
    break;

  case 65:

/* Line 1806 of yacc.c  */
#line 272 "xpctab.y"
    { (yyval.ep) = sum_epilogue((yyvsp[(5) - (6)].ep)); }
    break;

  case 70:

/* Line 1806 of yacc.c  */
#line 280 "xpctab.y"
    { do_lag((yyvsp[(1) - (1)].ep));}
    break;

  case 71:

/* Line 1806 of yacc.c  */
#line 283 "xpctab.y"
    {(yyval.ep) = add_elsepart((yyvsp[(1) - (3)].ep), (yyvsp[(3) - (3)].ep));}
    break;

  case 72:

/* Line 1806 of yacc.c  */
#line 286 "xpctab.y"
    {(yyval.ep) = (yyvsp[(1) - (1)].ep);}
    break;

  case 73:

/* Line 1806 of yacc.c  */
#line 287 "xpctab.y"
    {(yyval.ep) = add_elsepart((yyvsp[(1) - (2)].ep), (yyvsp[(2) - (2)].ep));}
    break;

  case 74:

/* Line 1806 of yacc.c  */
#line 289 "xpctab.y"
    {(yyval.ep) = mk_ifnode((yyvsp[(2) - (4)].ep), (yyvsp[(4) - (4)].ep));}
    break;

  case 75:

/* Line 1806 of yacc.c  */
#line 292 "xpctab.y"
    {(yyval.ep) = mk_ifnode((yyvsp[(2) - (4)].ep), (yyvsp[(4) - (4)].ep));}
    break;

  case 76:

/* Line 1806 of yacc.c  */
#line 295 "xpctab.y"
    {(yyval.ep) = (yyvsp[(1) - (1)].ep);}
    break;

  case 77:

/* Line 1806 of yacc.c  */
#line 296 "xpctab.y"
    {(yyval.ep) = add_elsepart((yyvsp[(1) - (2)].ep), (yyvsp[(2) - (2)].ep));}
    break;

  case 78:

/* Line 1806 of yacc.c  */
#line 299 "xpctab.y"
    { if( del_prologue((yyvsp[(1) - (1)].ival)) ) YYERROR; }
    break;

  case 79:

/* Line 1806 of yacc.c  */
#line 303 "xpctab.y"
    { if( sum_prologue((yyvsp[(1) - (5)].sp), (yyvsp[(3) - (5)].ival), (yyvsp[(5) - (5)].ival)) ) YYERROR; }
    break;

  case 80:

/* Line 1806 of yacc.c  */
#line 306 "xpctab.y"
    { (yyval.ep) = (yyvsp[(2) - (3)].ep); }
    break;

  case 81:

/* Line 1806 of yacc.c  */
#line 307 "xpctab.y"
    { Binary :
                                                  (yyval.ep) = mk_binenode((yyvsp[(2) - (3)].ival), (yyvsp[(1) - (3)].ep), (yyvsp[(3) - (3)].ep));
                                        }
    break;

  case 82:

/* Line 1806 of yacc.c  */
#line 310 "xpctab.y"
    { goto Binary; }
    break;

  case 83:

/* Line 1806 of yacc.c  */
#line 311 "xpctab.y"
    { goto Binary; }
    break;

  case 84:

/* Line 1806 of yacc.c  */
#line 312 "xpctab.y"
    { goto Binary; }
    break;

  case 85:

/* Line 1806 of yacc.c  */
#line 313 "xpctab.y"
    { goto Binary; }
    break;

  case 86:

/* Line 1806 of yacc.c  */
#line 314 "xpctab.y"
    { Unary :
                                                 (yyval.ep) = mk_unop((yyvsp[(1) - (2)].ival), (yyvsp[(2) - (2)].ep));
                                        }
    break;

  case 87:

/* Line 1806 of yacc.c  */
#line 317 "xpctab.y"
    { (yyval.ep) = (yyvsp[(2) - (2)].ep); }
    break;

  case 89:

/* Line 1806 of yacc.c  */
#line 321 "xpctab.y"
    { goto Binary; }
    break;

  case 90:

/* Line 1806 of yacc.c  */
#line 322 "xpctab.y"
    { goto Binary; }
    break;

  case 91:

/* Line 1806 of yacc.c  */
#line 323 "xpctab.y"
    { goto Binary; }
    break;

  case 92:

/* Line 1806 of yacc.c  */
#line 324 "xpctab.y"
    { goto Binary; }
    break;

  case 93:

/* Line 1806 of yacc.c  */
#line 325 "xpctab.y"
    { goto Binary; }
    break;

  case 94:

/* Line 1806 of yacc.c  */
#line 326 "xpctab.y"
    { goto Binary; }
    break;

  case 95:

/* Line 1806 of yacc.c  */
#line 327 "xpctab.y"
    { goto Binary; }
    break;

  case 96:

/* Line 1806 of yacc.c  */
#line 328 "xpctab.y"
    { goto Binary; }
    break;

  case 97:

/* Line 1806 of yacc.c  */
#line 329 "xpctab.y"
    { goto Binary; }
    break;

  case 98:

/* Line 1806 of yacc.c  */
#line 330 "xpctab.y"
    { goto Unary;  }
    break;

  case 99:

/* Line 1806 of yacc.c  */
#line 331 "xpctab.y"
    { (yyval.ep) = mk_unop(T_NOT, (yyvsp[(2) - (2)].ep)); }
    break;

  case 100:

/* Line 1806 of yacc.c  */
#line 338 "xpctab.y"
    { (yyval.ep) = mk_fargs(0 , (yyvsp[(1) - (1)].ep) ); }
    break;

  case 101:

/* Line 1806 of yacc.c  */
#line 339 "xpctab.y"
    { (yyval.ep) = mk_fargs((yyvsp[(1) - (3)].ep), (yyvsp[(3) - (3)].ep) ); }
    break;

  case 102:

/* Line 1806 of yacc.c  */
#line 346 "xpctab.y"
    { (yyval.ep) = mk_fargs(0 , (yyvsp[(1) - (1)].ep) ); }
    break;

  case 103:

/* Line 1806 of yacc.c  */
#line 347 "xpctab.y"
    { (yyval.ep) = mk_fargs((yyvsp[(1) - (3)].ep), (yyvsp[(3) - (3)].ep) ); }
    break;

  case 106:

/* Line 1806 of yacc.c  */
#line 363 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (1)].sp),  0, 0 , 0); }
    break;

  case 107:

/* Line 1806 of yacc.c  */
#line 364 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (4)].sp), (yyvsp[(3) - (4)].ival), 0 , 0); }
    break;

  case 108:

/* Line 1806 of yacc.c  */
#line 365 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (4)].sp),  0, (yyvsp[(3) - (4)].sp), 0); }
    break;

  case 109:

/* Line 1806 of yacc.c  */
#line 366 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (6)].sp),-(yyvsp[(5) - (6)].ival), (yyvsp[(3) - (6)].sp), 0); }
    break;

  case 110:

/* Line 1806 of yacc.c  */
#line 367 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (6)].sp), (yyvsp[(5) - (6)].ival), (yyvsp[(3) - (6)].sp), 0); }
    break;

  case 111:

/* Line 1806 of yacc.c  */
#line 370 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (4)].sp), (yyvsp[(3) - (4)].ival), 0 , 1); }
    break;

  case 112:

/* Line 1806 of yacc.c  */
#line 371 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (4)].sp),  0, (yyvsp[(3) - (4)].sp), 1); }
    break;

  case 113:

/* Line 1806 of yacc.c  */
#line 372 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (6)].sp),-(yyvsp[(5) - (6)].ival), (yyvsp[(3) - (6)].sp), 1); }
    break;

  case 114:

/* Line 1806 of yacc.c  */
#line 373 "xpctab.y"
    { (yyval.ep) = do_var((yyvsp[(1) - (6)].sp), (yyvsp[(5) - (6)].ival), (yyvsp[(3) - (6)].sp), 1); }
    break;

  case 115:

/* Line 1806 of yacc.c  */
#line 376 "xpctab.y"
    { (yyval.ival) = 0; }
    break;



/* Line 1806 of yacc.c  */
#line 2338 "xpctab.c"
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

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
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

  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

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

  *++yyvsp = yylval;


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

#if !defined(yyoverflow) || YYERROR_VERBOSE
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
  /* Do not reclaim the symbols of the rule which action triggered
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
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 2067 of yacc.c  */
#line 384 "xpctab.y"


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
        warning("Function name <%s> too long (max %d chars)\n",
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

#ifdef MCISIS
     if (!is_ulfunc) {
         if (errcnt == 0 && warncnt == 0) {
	     out_ipcode(sp);
         }
         fnp->fcp = NULL;
     }
#else
     new_eqn(sp);
#endif
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
        xpcerrmsg( "Use of function %s implies a nested DEL",
                    sp->name);
    }

    if (insum && sp->u.funp->flags.use_sum) {
        xpcerrmsg( "Use of function %s implies a nested SUM",
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
           for (p = eqn_name; (*p = toupper(*p)) != '\0'; p++);
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

#ifdef MCISIS
    if (errcnt == 0 && warncnt == 0) {
        out_ipcode(eqsp);
    }
    eqnp->ecp    = NULL;
#else
    new_eqn(eqsp);
#endif

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
        xpcerrmsg( "Nested SUM not allowed\n" );
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
        xpcerrmsg( "Nested DEL not allowed\n" );
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
#ifndef MCISIS
    argp->lower_del  = INT_MAX;
#endif
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
        warning("Variable name <%s> too long (max %d chars)\n",
                    sp->name , MAX_VARNLEN );

    sp->u.varp  = vp = emalloc( sizeof(Variable) );
    sp->xpctype = xpctype;

    vp->vtype   = vtype;
    vp->var_index = varCount++; /* variable index (first variable has index 0) */
#ifndef MCISIS
    vp->maxlead = 0;
    vp->maxlag  = 0;
#endif
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

static  void eqtyp_err(Symbol *sp) {
    xpcerrmsg("Illegal equation type %s", sp->name);
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
    /* adds a value to the current parameter. returns 0 if succesfull,
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

#ifdef MCISIS
     FNAME(save_parameter)((FINT *) &curpcnt, pval);
#else
     p = Curparam->u.parp;
     if (curpcnt == 1 ) {
         p->u.dval = pval[0];
     } else {
         p->u.dp = emalloc( sizeof(real) * curpcnt );
         memcpy( p->u.dp, pval, sizeof(real) * curpcnt );
     }
     new_par(Curparam);
#endif

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

