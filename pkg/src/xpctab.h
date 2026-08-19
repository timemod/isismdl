/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_MC_PKG_SRC_XPCTAB_H_INCLUDED
# define YY_MC_PKG_SRC_XPCTAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int mcdebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    T_END = 258,                   /* "end"  */
    T_FUNCTION = 259,              /* "function"  */
    T_UL_FUNCTION = 260,           /* "ul function"  */
    T_FRML = 261,                  /* "frml"  */
    T_IDENT = 262,                 /* "ident"  */
    T_PARAM = 263,                 /* "param"  */
    T_NAME = 264,                  /* "name"  */
    T_UFUNC = 265,                 /* "user function"  */
    T_ULFUNC = 266,                /* "user language function"  */
    T_IF = 267,                    /* "if"  */
    T_THEN = 268,                  /* "then"  */
    T_ELSE = 269,                  /* "else"  */
    T_ELSEIF = 270,                /* "elseif"  */
    T_ENDIF = 271,                 /* "endif"  */
    T_SUM = 272,                   /* "sum"  */
    T_DEL = 273,                   /* "del"  */
    T_BUILTIN = 274,               /* "builtin function"  */
    T_NUMBER = 275,                /* "number"  */
    T_INTNUM = 276,                /* "integer"  */
    T_GE = 277,                    /* ">="  */
    T_GT = 278,                    /* ">"  */
    T_LT = 279,                    /* "<"  */
    T_LE = 280,                    /* "<="  */
    T_EQ = 281,                    /* ".eq."  */
    T_NE = 282,                    /* "^="  */
    T_NOT = 283,                   /* "^ or .not."  */
    T_AND = 284,                   /* "& or .and."  */
    T_OR = 285,                    /* "| or .or."  */
    UNARYMINUS = 286,              /* UNARYMINUS  */
    T_POW = 287                    /* T_POW  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 62 "pkg/src/lex_yacc/xpctab.y"

        int     ival;
        real    dval;
        Symbol  *sp;
        Enodep  ep;

#line 103 "pkg/src/xpctab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE mclval;


int mcparse (void);


#endif /* !YY_MC_PKG_SRC_XPCTAB_H_INCLUDED  */
