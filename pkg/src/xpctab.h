/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

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
#line 62 "lex_yacc/xpctab.y" /* yacc.c:1909  */

        int     ival;
        real    dval;
        Symbol  *sp;
        Enodep  ep;

#line 125 "xpctab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE mclval;

int mcparse (void);

#endif /* !YY_MC_XPCTAB_H_INCLUDED  */
