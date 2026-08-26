/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

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

/* First part of user prologue.  */
#line 8 "pkg/src/lex_yacc/xpctab.y"


#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <Rinternals.h>

#include "ecode.h"
#include "mcinit.h"
#include "xpcdef.h"
#include "mchdr.h"
#include "util.h"
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


#line 129 "pkg/src/xpctab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "xpctab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_3_ = 3,                         /* '-'  */
  YYSYMBOL_4_ = 4,                         /* '+'  */
  YYSYMBOL_T_END = 5,                      /* "end"  */
  YYSYMBOL_T_FUNCTION = 6,                 /* "function"  */
  YYSYMBOL_T_UL_FUNCTION = 7,              /* "ul function"  */
  YYSYMBOL_T_FRML = 8,                     /* "frml"  */
  YYSYMBOL_T_IDENT = 9,                    /* "ident"  */
  YYSYMBOL_T_PARAM = 10,                   /* "param"  */
  YYSYMBOL_T_NAME = 11,                    /* "name"  */
  YYSYMBOL_T_UFUNC = 12,                   /* "user function"  */
  YYSYMBOL_T_ULFUNC = 13,                  /* "user language function"  */
  YYSYMBOL_T_IF = 14,                      /* "if"  */
  YYSYMBOL_T_THEN = 15,                    /* "then"  */
  YYSYMBOL_T_ELSE = 16,                    /* "else"  */
  YYSYMBOL_T_ELSEIF = 17,                  /* "elseif"  */
  YYSYMBOL_T_ENDIF = 18,                   /* "endif"  */
  YYSYMBOL_T_SUM = 19,                     /* "sum"  */
  YYSYMBOL_T_DEL = 20,                     /* "del"  */
  YYSYMBOL_T_BUILTIN = 21,                 /* "builtin function"  */
  YYSYMBOL_T_NUMBER = 22,                  /* "number"  */
  YYSYMBOL_T_INTNUM = 23,                  /* "integer"  */
  YYSYMBOL_T_GE = 24,                      /* ">="  */
  YYSYMBOL_T_GT = 25,                      /* ">"  */
  YYSYMBOL_T_LT = 26,                      /* "<"  */
  YYSYMBOL_T_LE = 27,                      /* "<="  */
  YYSYMBOL_T_EQ = 28,                      /* ".eq."  */
  YYSYMBOL_T_NE = 29,                      /* "^="  */
  YYSYMBOL_T_NOT = 30,                     /* "^ or .not."  */
  YYSYMBOL_T_AND = 31,                     /* "& or .and."  */
  YYSYMBOL_T_OR = 32,                      /* "| or .or."  */
  YYSYMBOL_33_ = 33,                       /* '^'  */
  YYSYMBOL_34_ = 34,                       /* '='  */
  YYSYMBOL_35_ = 35,                       /* '*'  */
  YYSYMBOL_36_ = 36,                       /* '/'  */
  YYSYMBOL_UNARYMINUS = 37,                /* UNARYMINUS  */
  YYSYMBOL_T_POW = 38,                     /* T_POW  */
  YYSYMBOL_39_ = 39,                       /* ';'  */
  YYSYMBOL_40_ = 40,                       /* '('  */
  YYSYMBOL_41_ = 41,                       /* ')'  */
  YYSYMBOL_42_ = 42,                       /* ','  */
  YYSYMBOL_43_ = 43,                       /* ':'  */
  YYSYMBOL_44_ = 44,                       /* '['  */
  YYSYMBOL_45_ = 45,                       /* ']'  */
  YYSYMBOL_YYACCEPT = 46,                  /* $accept  */
  YYSYMBOL_model = 47,                     /* model  */
  YYSYMBOL_opt_end = 48,                   /* opt_end  */
  YYSYMBOL_stmtlist = 49,                  /* stmtlist  */
  YYSYMBOL_stmt = 50,                      /* stmt  */
  YYSYMBOL_eqtyp = 51,                     /* eqtyp  */
  YYSYMBOL_eqname = 52,                    /* eqname  */
  YYSYMBOL_keywd = 53,                     /* keywd  */
  YYSYMBOL_plist = 54,                     /* plist  */
  YYSYMBOL_pnlist = 55,                    /* pnlist  */
  YYSYMBOL_pname = 56,                     /* pname  */
  YYSYMBOL_nlist = 57,                     /* nlist  */
  YYSYMBOL_snumber = 58,                   /* snumber  */
  YYSYMBOL_sinteger = 59,                  /* sinteger  */
  YYSYMBOL_lhsexpl = 60,                   /* lhsexpl  */
  YYSYMBOL_lhsimpl = 61,                   /* lhsimpl  */
  YYSYMBOL_funcname = 62,                  /* funcname  */
  YYSYMBOL_fstmt = 63,                     /* fstmt  */
  YYSYMBOL_64_1 = 64,                      /* $@1  */
  YYSYMBOL_ulfstmt = 65,                   /* ulfstmt  */
  YYSYMBOL_66_2 = 66,                      /* $@2  */
  YYSYMBOL_formals = 67,                   /* formals  */
  YYSYMBOL_sexpr = 68,                     /* sexpr  */
  YYSYMBOL_expr = 69,                      /* expr  */
  YYSYMBOL_ifexpr = 70,                    /* ifexpr  */
  YYSYMBOL_ifpart1 = 71,                   /* ifpart1  */
  YYSYMBOL_ifthen = 72,                    /* ifthen  */
  YYSYMBOL_elseif = 73,                    /* elseif  */
  YYSYMBOL_elseifl = 74,                   /* elseifl  */
  YYSYMBOL_delarg = 75,                    /* delarg  */
  YYSYMBOL_sumarg = 76,                    /* sumarg  */
  YYSYMBOL_rexpr = 77,                     /* rexpr  */
  YYSYMBOL_lexpr = 78,                     /* lexpr  */
  YYSYMBOL_fargs = 79,                     /* fargs  */
  YYSYMBOL_ufargs = 80,                    /* ufargs  */
  YYSYMBOL_ufarg = 81,                     /* ufarg  */
  YYSYMBOL_var = 82,                       /* var  */
  YYSYMBOL_opt_sign = 83                   /* opt_sign  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

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


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
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

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

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
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
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
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  27
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   442

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  46
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  38
/* YYNRULES -- Number of rules.  */
#define YYNRULES  115
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  221

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   287


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
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
static const yytype_int16 yyrline[] =
{
       0,   124,   124,   125,   128,   129,   132,   133,   136,   137,
     138,   139,   142,   146,   149,   152,   153,   156,   157,   158,
     161,   162,   164,   166,   170,   171,   172,   173,   174,   175,
     176,   179,   180,   183,   186,   189,   190,   193,   194,   197,
     204,   205,   207,   209,   213,   214,   217,   220,   230,   231,
     232,   235,   235,   239,   239,   243,   244,   253,   254,   256,
     257,   260,   264,   265,   267,   268,   269,   272,   273,   276,
     279,   280,   282,   285,   288,   289,   292,   295,   299,   300,
     303,   304,   305,   306,   307,   310,   311,   314,   315,   316,
     317,   318,   319,   320,   321,   322,   323,   324,   331,   332,
     339,   340,   346,   347,   356,   357,   358,   359,   360,   363,
     364,   365,   366,   369,   370,   371
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "'-'", "'+'",
  "\"end\"", "\"function\"", "\"ul function\"", "\"frml\"", "\"ident\"",
  "\"param\"", "\"name\"", "\"user function\"",
  "\"user language function\"", "\"if\"", "\"then\"", "\"else\"",
  "\"elseif\"", "\"endif\"", "\"sum\"", "\"del\"", "\"builtin function\"",
  "\"number\"", "\"integer\"", "\">=\"", "\">\"", "\"<\"", "\"<=\"",
  "\".eq.\"", "\"^=\"", "\"^ or .not.\"", "\"& or .and.\"",
  "\"| or .or.\"", "'^'", "'='", "'*'", "'/'", "UNARYMINUS", "T_POW",
  "';'", "'('", "')'", "','", "':'", "'['", "']'", "$accept", "model",
  "opt_end", "stmtlist", "stmt", "eqtyp", "eqname", "keywd", "plist",
  "pnlist", "pname", "nlist", "snumber", "sinteger", "lhsexpl", "lhsimpl",
  "funcname", "fstmt", "$@1", "ulfstmt", "$@2", "formals", "sexpr", "expr",
  "ifexpr", "ifpart1", "ifthen", "elseif", "elseifl", "delarg", "sumarg",
  "rexpr", "lexpr", "fargs", "ufargs", "ufarg", "var", "opt_sign", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-108)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-114)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      92,   -32,   -29,    69,    69,  -108,  -108,    16,  -108,    30,
    -108,    92,  -108,   168,  -108,  -108,  -108,  -108,  -108,  -108,
    -108,  -108,  -108,  -108,    -7,  -108,    73,  -108,  -108,  -108,
      -1,     5,    39,  -108,  -108,  -108,  -108,  -108,  -108,  -108,
      10,   278,    49,    54,    57,    56,    74,  -108,  -108,  -108,
    -108,   213,  -108,  -108,   135,   113,  -108,  -108,  -108,  -108,
      60,    84,   209,   209,   112,   112,  -108,  -108,  -108,    96,
     107,   110,   114,   209,   209,   209,   209,   -21,   120,   127,
     209,   130,   134,   146,  -108,  -108,   209,   209,   209,  -108,
      17,   172,   180,   186,  -108,  -108,  -108,   230,  -108,   151,
     153,  -108,  -108,  -108,  -108,   247,   284,   205,   205,    25,
      75,   209,   209,   302,   195,   187,   209,   404,   404,   118,
     209,   209,   209,   209,   209,   209,   209,   209,   209,   209,
     209,   209,   209,   209,  -108,  -108,   209,   209,  -108,   186,
    -108,   181,   233,  -108,  -108,  -108,    31,   204,   223,    12,
     202,   177,   353,   183,  -108,   185,   196,   209,   214,   210,
    -108,   217,   353,   199,  -108,     2,     2,    81,    81,    81,
      81,    81,    81,   404,   389,    81,   205,   205,   205,   353,
     338,  -108,   209,  -108,   229,   240,  -108,  -108,   244,   254,
    -108,  -108,  -108,   209,  -108,   353,    73,   209,   209,  -108,
     209,   209,   353,   239,   243,   225,   251,  -108,   258,   137,
     173,   353,   353,  -108,  -108,  -108,  -108,    73,  -108,  -108,
    -108
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     0,     0,    17,    18,     0,    15,     0,
       3,     0,     6,     0,    16,     5,    48,    49,    50,    51,
       8,    53,     9,    34,     0,    31,   113,     1,     2,     7,
      20,    22,    23,    27,    30,    28,    29,    25,    26,    24,
       0,     0,    21,     0,     0,     0,     0,    10,    32,   114,
     115,    33,    35,    38,     0,     0,    40,    41,    42,    43,
       0,     0,     0,     0,     0,     0,    36,    37,    39,     0,
       0,     0,     0,     0,     0,     0,     0,   104,     0,     0,
       0,     0,     0,     0,    57,    58,     0,     0,     0,    67,
       0,    64,     0,    70,    66,    86,    68,     0,    55,     0,
       0,    44,    45,    46,    47,     0,     0,    84,    85,   113,
     113,     0,     0,     0,     0,     0,     0,    96,    97,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    11,    65,     0,     0,    74,    71,
      12,     0,     0,    54,    13,    14,     0,     0,     0,     0,
       0,    67,     0,     0,   100,    68,     0,     0,     0,     0,
      76,     0,    98,     0,    78,    80,    79,    88,    87,    89,
      90,    92,    93,    94,    95,    91,    81,    82,    83,    69,
       0,    75,     0,    56,     0,     0,   110,   109,     0,     0,
     106,   105,    60,     0,    61,    72,   113,     0,     0,    59,
       0,     0,    52,     0,     0,     0,     0,   101,     0,     0,
       0,    99,    73,   111,   112,   107,   108,   113,    63,    62,
      77
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -108,  -108,   291,  -108,   292,  -108,  -108,   -24,  -108,   280,
    -108,  -108,   256,  -107,   273,   283,   317,  -108,  -108,  -108,
    -108,   260,  -106,   -62,  -108,  -108,  -108,   193,  -108,  -108,
    -108,  -108,  -108,  -108,   227,   142,  -103,    -4
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     9,    10,    11,    12,    13,    41,    42,    24,    25,
      26,    51,    52,    53,    43,    44,    19,    20,    45,    22,
      46,    99,    89,   152,    91,    92,    93,   138,   139,   161,
     159,    94,    95,   163,   153,   154,    96,   148
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      90,    97,   147,   150,    23,   151,   151,    14,   155,   155,
      15,   105,   106,   107,   108,   188,   189,    59,   113,   109,
     120,   121,    54,   110,   117,   118,   119,    23,    49,    50,
      27,    72,    47,   -40,   184,   185,   146,   131,   132,   -41,
     133,   122,   123,   124,   125,   126,   127,    54,   128,   129,
      55,   130,   131,   132,   162,   133,   134,   190,   165,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     177,   178,   186,   -42,   179,   180,    49,    50,    49,    50,
      16,    17,    18,   -43,   120,   121,   149,   151,    62,   208,
     155,    63,    -4,     1,    73,   195,    64,     2,     3,     4,
       5,     6,     7,   -19,   -19,   -19,   -19,   -19,   -19,   -19,
     220,   -19,   -19,   -19,    65,   -19,   131,   132,    74,   133,
     202,   120,   121,    98,    69,    70,    71,    33,    34,    35,
      36,     8,    37,    38,    39,   209,   210,   101,   211,   212,
     120,   121,   122,   123,   124,   125,   126,   127,   102,   128,
     129,   103,   130,   131,   132,   104,   133,    67,    68,   164,
     111,   122,   123,   124,   125,   126,   127,   112,   128,   129,
     114,   130,   131,   132,   115,   133,   120,   121,   218,    30,
      31,    32,    33,    34,    35,    36,   116,    37,    38,    39,
     135,    40,   141,   142,   143,   142,   136,   122,   123,   124,
     125,   126,   127,   137,   128,   129,   158,   130,   131,   132,
     160,   133,    75,    76,   219,   182,    49,    50,  -102,  -102,
      77,    78,    79,    80,   192,   193,  -103,  -103,    81,    82,
      83,    84,    85,   120,   121,  -113,  -113,   194,   193,    86,
     199,   200,    87,   133,   183,   187,    68,   191,   196,    88,
     120,   121,   203,   197,   122,   123,   124,   125,   126,   127,
     198,   128,   129,   204,   130,   131,   132,   205,   133,   140,
     215,   122,   123,   124,   125,   126,   127,   206,   128,   129,
     213,   130,   131,   132,   214,   133,   144,   120,   121,    56,
      57,    58,    33,    34,    35,    36,   216,    37,    38,    39,
     217,    40,    28,    29,    48,   120,   121,    66,   122,   123,
     124,   125,   126,   127,    60,   128,   129,   157,   130,   131,
     132,    21,   133,   145,    61,   100,   122,   123,   124,   125,
     126,   127,   181,   128,   129,   207,   130,   131,   132,   156,
     133,   120,   121,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   120,   121,     0,     0,
       0,     0,   122,   123,   124,   125,   126,   127,     0,   128,
     129,     0,   130,   131,   132,     0,   133,   122,   123,   124,
     125,   126,   127,     0,   128,   129,     0,   130,   131,   132,
       0,   133,   120,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   121,     0,
       0,     0,     0,   122,   123,   124,   125,   126,   127,     0,
     128,     0,     0,   130,   131,   132,     0,   133,   122,   123,
     124,   125,   126,   127,     0,     0,     0,     0,   130,   131,
     132,     0,   133
};

static const yytype_int16 yycheck[] =
{
      62,    63,   109,   110,    11,   111,   112,    39,   111,   112,
      39,    73,    74,    75,    76,     3,     4,    41,    80,    40,
       3,     4,    26,    44,    86,    87,    88,    11,     3,     4,
       0,    55,    39,    34,     3,     4,    11,    35,    36,    34,
      38,    24,    25,    26,    27,    28,    29,    51,    31,    32,
      40,    34,    35,    36,   116,    38,    39,    45,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    41,    34,   136,   137,     3,     4,     3,     4,
      11,    12,    13,    34,     3,     4,    11,   193,    34,   196,
     193,    34,     0,     1,    34,   157,    40,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
     217,    19,    20,    21,    40,    23,    35,    36,    34,    38,
     182,     3,     4,    11,    11,    12,    13,    14,    15,    16,
      17,    39,    19,    20,    21,   197,   198,    41,   200,   201,
       3,     4,    24,    25,    26,    27,    28,    29,    41,    31,
      32,    41,    34,    35,    36,    41,    38,    22,    23,    41,
      40,    24,    25,    26,    27,    28,    29,    40,    31,    32,
      40,    34,    35,    36,    40,    38,     3,     4,    41,    11,
      12,    13,    14,    15,    16,    17,    40,    19,    20,    21,
      18,    23,    41,    42,    41,    42,    16,    24,    25,    26,
      27,    28,    29,    17,    31,    32,    11,    34,    35,    36,
      23,    38,     3,     4,    41,    34,     3,     4,    41,    42,
      11,    12,    13,    14,    41,    42,    41,    42,    19,    20,
      21,    22,    23,     3,     4,    22,    23,    41,    42,    30,
      41,    42,    33,    38,    11,    41,    23,    45,    34,    40,
       3,     4,    23,    43,    24,    25,    26,    27,    28,    29,
      43,    31,    32,    23,    34,    35,    36,    23,    38,    39,
      45,    24,    25,    26,    27,    28,    29,    23,    31,    32,
      41,    34,    35,    36,    41,    38,    39,     3,     4,    11,
      12,    13,    14,    15,    16,    17,    45,    19,    20,    21,
      42,    23,    11,    11,    24,     3,     4,    51,    24,    25,
      26,    27,    28,    29,    41,    31,    32,    15,    34,    35,
      36,     4,    38,    39,    41,    65,    24,    25,    26,    27,
      28,    29,   139,    31,    32,   193,    34,    35,    36,   112,
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

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     5,     6,     7,     8,     9,    10,    39,    47,
      48,    49,    50,    51,    39,    39,    11,    12,    13,    62,
      63,    62,    65,    11,    54,    55,    56,     0,    48,    50,
      11,    12,    13,    14,    15,    16,    17,    19,    20,    21,
      23,    52,    53,    60,    61,    64,    66,    39,    55,     3,
       4,    57,    58,    59,    83,    40,    11,    12,    13,    53,
      60,    61,    34,    34,    40,    40,    58,    22,    23,    11,
      12,    13,    53,    34,    34,     3,     4,    11,    12,    13,
      14,    19,    20,    21,    22,    23,    30,    33,    40,    68,
      69,    70,    71,    72,    77,    78,    82,    69,    11,    67,
      67,    41,    41,    41,    41,    69,    69,    69,    69,    40,
      44,    40,    40,    69,    40,    40,    40,    69,    69,    69,
       3,     4,    24,    25,    26,    27,    28,    29,    31,    32,
      34,    35,    36,    38,    39,    18,    16,    17,    73,    74,
      39,    41,    42,    41,    39,    39,    11,    59,    83,    11,
      59,    68,    69,    80,    81,    82,    80,    15,    11,    76,
      23,    75,    69,    79,    41,    69,    69,    69,    69,    69,
      69,    69,    69,    69,    69,    69,    69,    69,    69,    69,
      69,    73,    34,    11,     3,     4,    41,    41,     3,     4,
      45,    45,    41,    42,    41,    69,    34,    43,    43,    41,
      42,    15,    69,    23,    23,    23,    23,    81,    59,    69,
      69,    69,    69,    41,    41,    45,    45,    42,    41,    41,
      59
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    46,    47,    47,    48,    48,    49,    49,    50,    50,
      50,    50,    50,    50,    50,    50,    50,    51,    51,    51,
      52,    52,    52,    52,    53,    53,    53,    53,    53,    53,
      53,    54,    54,    55,    56,    57,    57,    58,    58,    59,
      60,    60,    60,    60,    61,    61,    61,    61,    62,    62,
      62,    64,    63,    66,    65,    67,    67,    68,    68,    68,
      68,    68,    68,    68,    68,    68,    68,    69,    69,    70,
      71,    71,    72,    73,    74,    74,    75,    76,    77,    77,
      77,    77,    77,    77,    77,    77,    77,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    79,    79,
      80,    80,    81,    81,    82,    82,    82,    82,    82,    82,
      82,    82,    82,    83,    83,    83
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     1,     0,     2,     1,     2,     2,     2,
       3,     5,     5,     6,     6,     1,     2,     1,     1,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     1,     1,     2,     2,     1,     2,
       1,     1,     1,     1,     4,     4,     4,     4,     1,     1,
       1,     0,     7,     0,     5,     1,     3,     1,     1,     4,
       4,     4,     6,     6,     1,     2,     1,     1,     1,     3,
       1,     2,     4,     4,     1,     2,     1,     5,     3,     3,
       3,     3,     3,     3,     2,     2,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     2,     2,     1,     3,
       1,     3,     1,     1,     1,     4,     4,     6,     6,     4,
       4,     6,     6,     0,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
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

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


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




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
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
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
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


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
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
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
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
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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
            else
              goto append;

          append:
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
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
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
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
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
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
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
| yyreduce -- do a reduction.  |
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
  case 5: /* opt_end: "end" ';'  */
#line 129 "pkg/src/lex_yacc/xpctab.y"
                    {YYACCEPT;}
#line 1663 "pkg/src/xpctab.c"
    break;

  case 11: /* stmt: eqtyp lhsexpl '=' expr ';'  */
#line 140 "pkg/src/lex_yacc/xpctab.y"
                                  { eqn_epilogue(NULL, (yyvsp[-4].ival), (yyvsp[-3].sp), (yyvsp[-1].ep), 0);}
#line 1669 "pkg/src/xpctab.c"
    break;

  case 12: /* stmt: eqtyp lhsimpl '=' expr ';'  */
#line 143 "pkg/src/lex_yacc/xpctab.y"
                                  { eqn_epilogue(NULL, (yyvsp[-4].ival), (yyvsp[-3].sp), (yyvsp[-1].ep), 1);}
#line 1675 "pkg/src/xpctab.c"
    break;

  case 13: /* stmt: eqtyp eqname lhsexpl '=' expr ';'  */
#line 147 "pkg/src/lex_yacc/xpctab.y"
                                  { eqn_epilogue((yyvsp[-4].sp), (yyvsp[-5].ival), (yyvsp[-3].sp), (yyvsp[-1].ep), 0);}
#line 1681 "pkg/src/xpctab.c"
    break;

  case 14: /* stmt: eqtyp eqname lhsimpl '=' expr ';'  */
#line 150 "pkg/src/lex_yacc/xpctab.y"
                                  { eqn_epilogue((yyvsp[-4].sp), (yyvsp[-5].ival), (yyvsp[-3].sp), (yyvsp[-1].ep), 1);}
#line 1687 "pkg/src/xpctab.c"
    break;

  case 16: /* stmt: error ';'  */
#line 153 "pkg/src/lex_yacc/xpctab.y"
                                  { cleanup(0); yyerrok; }
#line 1693 "pkg/src/xpctab.c"
    break;

  case 17: /* eqtyp: "frml"  */
#line 156 "pkg/src/lex_yacc/xpctab.y"
                        { eqn_prologue(); (yyval.ival) = 1;}
#line 1699 "pkg/src/xpctab.c"
    break;

  case 18: /* eqtyp: "ident"  */
#line 157 "pkg/src/lex_yacc/xpctab.y"
                        { eqn_prologue(); (yyval.ival) = 0;}
#line 1705 "pkg/src/xpctab.c"
    break;

  case 19: /* eqtyp: %empty  */
#line 158 "pkg/src/lex_yacc/xpctab.y"
                        { eqn_prologue(); (yyval.ival) = 0;}
#line 1711 "pkg/src/xpctab.c"
    break;

  case 24: /* keywd: "builtin function"  */
#line 170 "pkg/src/lex_yacc/xpctab.y"
                      {(yyval.sp)=(yyvsp[0].sp);}
#line 1717 "pkg/src/xpctab.c"
    break;

  case 25: /* keywd: "sum"  */
#line 171 "pkg/src/lex_yacc/xpctab.y"
                      {(yyval.sp)=(yyvsp[0].sp);}
#line 1723 "pkg/src/xpctab.c"
    break;

  case 26: /* keywd: "del"  */
#line 172 "pkg/src/lex_yacc/xpctab.y"
                      {(yyval.sp)=(yyvsp[0].sp);}
#line 1729 "pkg/src/xpctab.c"
    break;

  case 27: /* keywd: "if"  */
#line 173 "pkg/src/lex_yacc/xpctab.y"
                      {(yyval.sp)=(yyvsp[0].sp);}
#line 1735 "pkg/src/xpctab.c"
    break;

  case 28: /* keywd: "else"  */
#line 174 "pkg/src/lex_yacc/xpctab.y"
                      {(yyval.sp)=(yyvsp[0].sp);}
#line 1741 "pkg/src/xpctab.c"
    break;

  case 29: /* keywd: "elseif"  */
#line 175 "pkg/src/lex_yacc/xpctab.y"
                      {(yyval.sp)=(yyvsp[0].sp);}
#line 1747 "pkg/src/xpctab.c"
    break;

  case 30: /* keywd: "then"  */
#line 176 "pkg/src/lex_yacc/xpctab.y"
                      {(yyval.sp)=(yyvsp[0].sp);}
#line 1753 "pkg/src/xpctab.c"
    break;

  case 33: /* pnlist: pname nlist  */
#line 183 "pkg/src/lex_yacc/xpctab.y"
                        { set_parval(); }
#line 1759 "pkg/src/xpctab.c"
    break;

  case 34: /* pname: "name"  */
#line 186 "pkg/src/lex_yacc/xpctab.y"
                        { make_param((yyvsp[0].sp));}
#line 1765 "pkg/src/xpctab.c"
    break;

  case 35: /* nlist: snumber  */
#line 189 "pkg/src/lex_yacc/xpctab.y"
                         { if (add_pval((yyvsp[0].dval))) YYERROR;}
#line 1771 "pkg/src/xpctab.c"
    break;

  case 36: /* nlist: nlist snumber  */
#line 190 "pkg/src/lex_yacc/xpctab.y"
                         { if (add_pval((yyvsp[0].dval))) YYERROR;}
#line 1777 "pkg/src/xpctab.c"
    break;

  case 37: /* snumber: opt_sign "number"  */
#line 193 "pkg/src/lex_yacc/xpctab.y"
                            { (yyval.dval) = ((yyvsp[-1].ival) == '-') ? -(yyvsp[0].dval) : (yyvsp[0].dval); }
#line 1783 "pkg/src/xpctab.c"
    break;

  case 38: /* snumber: sinteger  */
#line 194 "pkg/src/lex_yacc/xpctab.y"
                             {(yyval.dval) = (yyvsp[0].ival);}
#line 1789 "pkg/src/xpctab.c"
    break;

  case 39: /* sinteger: opt_sign "integer"  */
#line 197 "pkg/src/lex_yacc/xpctab.y"
                             { (yyval.ival) = ((yyvsp[-1].ival) == '-') ? -(yyvsp[0].ival) : (yyvsp[0].ival); }
#line 1795 "pkg/src/xpctab.c"
    break;

  case 40: /* lhsexpl: "name"  */
#line 204 "pkg/src/lex_yacc/xpctab.y"
                                  { check_lhs((yyvsp[0].sp)); }
#line 1801 "pkg/src/xpctab.c"
    break;

  case 41: /* lhsexpl: "user function"  */
#line 206 "pkg/src/lex_yacc/xpctab.y"
                                  { check_lhs((yyvsp[0].sp)); }
#line 1807 "pkg/src/xpctab.c"
    break;

  case 42: /* lhsexpl: "user language function"  */
#line 208 "pkg/src/lex_yacc/xpctab.y"
                                  { check_lhs((yyvsp[0].sp)); }
#line 1813 "pkg/src/xpctab.c"
    break;

  case 43: /* lhsexpl: keywd  */
#line 210 "pkg/src/lex_yacc/xpctab.y"
                                  { check_lhs((yyvsp[0].sp)); }
#line 1819 "pkg/src/xpctab.c"
    break;

  case 44: /* lhsimpl: "integer" '(' "name" ')'  */
#line 213 "pkg/src/lex_yacc/xpctab.y"
                                   { (yyval.sp) = (yyvsp[-1].sp); if(check_implicit((yyvsp[-3].ival), (yyvsp[-1].sp))) YYERROR; }
#line 1825 "pkg/src/xpctab.c"
    break;

  case 45: /* lhsimpl: "integer" '(' "user function" ')'  */
#line 216 "pkg/src/lex_yacc/xpctab.y"
                                { (yyval.sp) = (yyvsp[-1].sp); if(check_implicit((yyvsp[-3].ival), (yyvsp[-1].sp))) YYERROR; }
#line 1831 "pkg/src/xpctab.c"
    break;

  case 46: /* lhsimpl: "integer" '(' "user language function" ')'  */
#line 219 "pkg/src/lex_yacc/xpctab.y"
                                { (yyval.sp) = (yyvsp[-1].sp); if(check_implicit((yyvsp[-3].ival), (yyvsp[-1].sp))) YYERROR; }
#line 1837 "pkg/src/xpctab.c"
    break;

  case 47: /* lhsimpl: "integer" '(' keywd ')'  */
#line 222 "pkg/src/lex_yacc/xpctab.y"
                                { (yyval.sp) = (yyvsp[-1].sp); if(check_implicit((yyvsp[-3].ival), (yyvsp[-1].sp))) YYERROR; }
#line 1843 "pkg/src/xpctab.c"
    break;

  case 51: /* $@1: %empty  */
#line 235 "pkg/src/lex_yacc/xpctab.y"
                                   { if (ufunc_prologue((yyvsp[0].sp), 0)) YYERROR;}
#line 1849 "pkg/src/xpctab.c"
    break;

  case 52: /* fstmt: funcname $@1 '(' formals ')' '=' expr  */
#line 236 "pkg/src/lex_yacc/xpctab.y"
                                   { ufunc_epilogue((yyvsp[-6].sp), (yyvsp[0].ep), 0); }
#line 1855 "pkg/src/xpctab.c"
    break;

  case 53: /* $@2: %empty  */
#line 239 "pkg/src/lex_yacc/xpctab.y"
                          { if (ufunc_prologue((yyvsp[0].sp), 1)) YYERROR;}
#line 1861 "pkg/src/xpctab.c"
    break;

  case 54: /* ulfstmt: funcname $@2 '(' formals ')'  */
#line 240 "pkg/src/lex_yacc/xpctab.y"
                          { ufunc_epilogue((yyvsp[-4].sp), 0, 1); }
#line 1867 "pkg/src/xpctab.c"
    break;

  case 55: /* formals: "name"  */
#line 243 "pkg/src/lex_yacc/xpctab.y"
                                   { make_formal((yyvsp[0].sp)); }
#line 1873 "pkg/src/xpctab.c"
    break;

  case 56: /* formals: formals ',' "name"  */
#line 244 "pkg/src/lex_yacc/xpctab.y"
                                   { make_formal((yyvsp[0].sp)); }
#line 1879 "pkg/src/xpctab.c"
    break;

  case 57: /* sexpr: "number"  */
#line 253 "pkg/src/lex_yacc/xpctab.y"
                                           { (yyval.ep) = mk_const((yyvsp[0].dval)); }
#line 1885 "pkg/src/xpctab.c"
    break;

  case 58: /* sexpr: "integer"  */
#line 254 "pkg/src/lex_yacc/xpctab.y"
                                           { (yyval.ep) = mk_const((yyvsp[0].ival)); }
#line 1891 "pkg/src/xpctab.c"
    break;

  case 59: /* sexpr: "builtin function" '(' fargs ')'  */
#line 256 "pkg/src/lex_yacc/xpctab.y"
                                           { (yyval.ep) = do_builtin((yyvsp[-3].sp), (yyvsp[-1].ep)); }
#line 1897 "pkg/src/xpctab.c"
    break;

  case 60: /* sexpr: "user function" '(' ufargs ')'  */
#line 257 "pkg/src/lex_yacc/xpctab.y"
                                           { 
                              if (((yyval.ep) = do_funccall((yyvsp[-3].sp), (yyvsp[-1].ep))) == 0) YYERROR;
                                           }
#line 1905 "pkg/src/xpctab.c"
    break;

  case 61: /* sexpr: "user language function" '(' ufargs ')'  */
#line 260 "pkg/src/lex_yacc/xpctab.y"
                                            { 
                              if (((yyval.ep) = do_funccall((yyvsp[-3].sp), (yyvsp[-1].ep))) == 0) YYERROR;
                                           }
#line 1913 "pkg/src/xpctab.c"
    break;

  case 62: /* sexpr: "del" '(' delarg ':' expr ')'  */
#line 264 "pkg/src/lex_yacc/xpctab.y"
                                           { (yyval.ep) = del_epilogue((yyvsp[-1].ep)); }
#line 1919 "pkg/src/xpctab.c"
    break;

  case 63: /* sexpr: "sum" '(' sumarg ':' expr ')'  */
#line 265 "pkg/src/lex_yacc/xpctab.y"
                                           { (yyval.ep) = sum_epilogue((yyvsp[-1].ep)); }
#line 1925 "pkg/src/xpctab.c"
    break;

  case 68: /* expr: var  */
#line 273 "pkg/src/lex_yacc/xpctab.y"
                                    {do_lag((yyvsp[0].ep));}
#line 1931 "pkg/src/xpctab.c"
    break;

  case 69: /* ifexpr: ifpart1 "else" expr  */
#line 276 "pkg/src/lex_yacc/xpctab.y"
                                    {(yyval.ep) = add_elsepart((yyvsp[-2].ep), (yyvsp[0].ep));}
#line 1937 "pkg/src/xpctab.c"
    break;

  case 70: /* ifpart1: ifthen  */
#line 279 "pkg/src/lex_yacc/xpctab.y"
                                    {(yyval.ep) = (yyvsp[0].ep);}
#line 1943 "pkg/src/xpctab.c"
    break;

  case 71: /* ifpart1: ifthen elseifl  */
#line 280 "pkg/src/lex_yacc/xpctab.y"
                                    {(yyval.ep) = add_elsepart((yyvsp[-1].ep), (yyvsp[0].ep));}
#line 1949 "pkg/src/xpctab.c"
    break;

  case 72: /* ifthen: "if" expr "then" expr  */
#line 282 "pkg/src/lex_yacc/xpctab.y"
                                    {(yyval.ep) = mk_ifnode((yyvsp[-2].ep), (yyvsp[0].ep));}
#line 1955 "pkg/src/xpctab.c"
    break;

  case 73: /* elseif: "elseif" expr "then" expr  */
#line 285 "pkg/src/lex_yacc/xpctab.y"
                                    {(yyval.ep) = mk_ifnode((yyvsp[-2].ep), (yyvsp[0].ep));}
#line 1961 "pkg/src/xpctab.c"
    break;

  case 74: /* elseifl: elseif  */
#line 288 "pkg/src/lex_yacc/xpctab.y"
                          {(yyval.ep) = (yyvsp[0].ep);}
#line 1967 "pkg/src/xpctab.c"
    break;

  case 75: /* elseifl: elseifl elseif  */
#line 289 "pkg/src/lex_yacc/xpctab.y"
                          {(yyval.ep) = add_elsepart((yyvsp[-1].ep), (yyvsp[0].ep));}
#line 1973 "pkg/src/xpctab.c"
    break;

  case 76: /* delarg: "integer"  */
#line 292 "pkg/src/lex_yacc/xpctab.y"
                   { if( del_prologue((yyvsp[0].ival)) ) YYERROR; }
#line 1979 "pkg/src/xpctab.c"
    break;

  case 77: /* sumarg: "name" '=' sinteger ',' sinteger  */
#line 296 "pkg/src/lex_yacc/xpctab.y"
                                       { if( sum_prologue((yyvsp[-4].sp), (yyvsp[-2].ival), (yyvsp[0].ival)) ) YYERROR; }
#line 1985 "pkg/src/xpctab.c"
    break;

  case 78: /* rexpr: '(' expr ')'  */
#line 299 "pkg/src/lex_yacc/xpctab.y"
                                        { (yyval.ep) = (yyvsp[-1].ep); }
#line 1991 "pkg/src/xpctab.c"
    break;

  case 79: /* rexpr: expr '+' expr  */
#line 300 "pkg/src/lex_yacc/xpctab.y"
                                        { Binary :
                                                  (yyval.ep) = mk_binenode((yyvsp[-1].ival), (yyvsp[-2].ep), (yyvsp[0].ep));
                                        }
#line 1999 "pkg/src/xpctab.c"
    break;

  case 80: /* rexpr: expr '-' expr  */
#line 303 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2005 "pkg/src/xpctab.c"
    break;

  case 81: /* rexpr: expr '*' expr  */
#line 304 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2011 "pkg/src/xpctab.c"
    break;

  case 82: /* rexpr: expr '/' expr  */
#line 305 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2017 "pkg/src/xpctab.c"
    break;

  case 83: /* rexpr: expr T_POW expr  */
#line 306 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2023 "pkg/src/xpctab.c"
    break;

  case 84: /* rexpr: '-' expr  */
#line 307 "pkg/src/lex_yacc/xpctab.y"
                                        { Unary :
                                                 (yyval.ep) = mk_unop((yyvsp[-1].ival), (yyvsp[0].ep));
                                        }
#line 2031 "pkg/src/xpctab.c"
    break;

  case 85: /* rexpr: '+' expr  */
#line 310 "pkg/src/lex_yacc/xpctab.y"
                                        { (yyval.ep) = (yyvsp[0].ep); }
#line 2037 "pkg/src/xpctab.c"
    break;

  case 87: /* lexpr: expr ">" expr  */
#line 314 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2043 "pkg/src/xpctab.c"
    break;

  case 88: /* lexpr: expr ">=" expr  */
#line 315 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2049 "pkg/src/xpctab.c"
    break;

  case 89: /* lexpr: expr "<" expr  */
#line 316 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2055 "pkg/src/xpctab.c"
    break;

  case 90: /* lexpr: expr "<=" expr  */
#line 317 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2061 "pkg/src/xpctab.c"
    break;

  case 91: /* lexpr: expr '=' expr  */
#line 318 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2067 "pkg/src/xpctab.c"
    break;

  case 92: /* lexpr: expr ".eq." expr  */
#line 319 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2073 "pkg/src/xpctab.c"
    break;

  case 93: /* lexpr: expr "^=" expr  */
#line 320 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2079 "pkg/src/xpctab.c"
    break;

  case 94: /* lexpr: expr "& or .and." expr  */
#line 321 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2085 "pkg/src/xpctab.c"
    break;

  case 95: /* lexpr: expr "| or .or." expr  */
#line 322 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Binary; }
#line 2091 "pkg/src/xpctab.c"
    break;

  case 96: /* lexpr: "^ or .not." expr  */
#line 323 "pkg/src/lex_yacc/xpctab.y"
                                        { goto Unary;  }
#line 2097 "pkg/src/xpctab.c"
    break;

  case 97: /* lexpr: '^' expr  */
#line 324 "pkg/src/lex_yacc/xpctab.y"
                                        { (yyval.ep) = mk_unop(T_NOT, (yyvsp[0].ep)); }
#line 2103 "pkg/src/xpctab.c"
    break;

  case 98: /* fargs: expr  */
#line 331 "pkg/src/lex_yacc/xpctab.y"
                         { (yyval.ep) = mk_fargs(0 , (yyvsp[0].ep) ); }
#line 2109 "pkg/src/xpctab.c"
    break;

  case 99: /* fargs: fargs ',' expr  */
#line 332 "pkg/src/lex_yacc/xpctab.y"
                         { (yyval.ep) = mk_fargs((yyvsp[-2].ep), (yyvsp[0].ep) ); }
#line 2115 "pkg/src/xpctab.c"
    break;

  case 100: /* ufargs: ufarg  */
#line 339 "pkg/src/lex_yacc/xpctab.y"
                             { (yyval.ep) = mk_fargs(0 , (yyvsp[0].ep) ); }
#line 2121 "pkg/src/xpctab.c"
    break;

  case 101: /* ufargs: ufargs ',' ufarg  */
#line 340 "pkg/src/lex_yacc/xpctab.y"
                             { (yyval.ep) = mk_fargs((yyvsp[-2].ep), (yyvsp[0].ep) ); }
#line 2127 "pkg/src/xpctab.c"
    break;

  case 104: /* var: "name"  */
#line 356 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[0].sp),  0, 0 , 0); }
#line 2133 "pkg/src/xpctab.c"
    break;

  case 105: /* var: "name" '[' sinteger ']'  */
#line 357 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[-3].sp), (yyvsp[-1].ival), 0 , 0); }
#line 2139 "pkg/src/xpctab.c"
    break;

  case 106: /* var: "name" '[' "name" ']'  */
#line 358 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[-3].sp),  0, (yyvsp[-1].sp), 0); }
#line 2145 "pkg/src/xpctab.c"
    break;

  case 107: /* var: "name" '[' "name" '-' "integer" ']'  */
#line 359 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[-5].sp),-(yyvsp[-1].ival), (yyvsp[-3].sp), 0); }
#line 2151 "pkg/src/xpctab.c"
    break;

  case 108: /* var: "name" '[' "name" '+' "integer" ']'  */
#line 360 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[-5].sp), (yyvsp[-1].ival), (yyvsp[-3].sp), 0); }
#line 2157 "pkg/src/xpctab.c"
    break;

  case 109: /* var: "name" '(' sinteger ')'  */
#line 363 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[-3].sp), (yyvsp[-1].ival), 0 , 1); }
#line 2163 "pkg/src/xpctab.c"
    break;

  case 110: /* var: "name" '(' "name" ')'  */
#line 364 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[-3].sp),  0, (yyvsp[-1].sp), 1); }
#line 2169 "pkg/src/xpctab.c"
    break;

  case 111: /* var: "name" '(' "name" '-' "integer" ')'  */
#line 365 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[-5].sp),-(yyvsp[-1].ival), (yyvsp[-3].sp), 1); }
#line 2175 "pkg/src/xpctab.c"
    break;

  case 112: /* var: "name" '(' "name" '+' "integer" ')'  */
#line 366 "pkg/src/lex_yacc/xpctab.y"
                                               { (yyval.ep) = do_var((yyvsp[-5].sp), (yyvsp[-1].ival), (yyvsp[-3].sp), 1); }
#line 2181 "pkg/src/xpctab.c"
    break;

  case 113: /* opt_sign: %empty  */
#line 369 "pkg/src/lex_yacc/xpctab.y"
                    { (yyval.ival) = 0; }
#line 2187 "pkg/src/xpctab.c"
    break;


#line 2191 "pkg/src/xpctab.c"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
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
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

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

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
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
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
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
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 377 "pkg/src/lex_yacc/xpctab.y"


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
    deloffset = 0;
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

/*
 * create a parameter variable
 */
void make_param(Symbol *sp) {

    curpcnt = 0;
    Curparam = sp;

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

    /* create a Param, and set its length */
    Curparam->u.parp = p = emalloc( sizeof(Param) );
    p->cnt = curpcnt;
    p->par_index = parCount++; /* variable index (first variable has index 0) */

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
