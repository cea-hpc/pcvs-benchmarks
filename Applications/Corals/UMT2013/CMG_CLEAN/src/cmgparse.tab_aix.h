/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     SEEDTOK = 258,
     SMSTOK = 259,
     BLKTOK = 260,
     STATE = 261,
     NUMZONESTOK = 262,
     SUBTOK = 263,
     TAGTOK = 264,
     MESHTAGTYPE = 265,
     MATERIALTOK = 266,
     INDEXMODE = 267,
     SUBTYPE = 268,
     NUMBER = 269,
     WORD = 270,
     NAME = 271,
     COMMENT = 272
   };
#endif
/* Tokens.  */
#define SEEDTOK 258
#define SMSTOK 259
#define BLKTOK 260
#define STATE 261
#define NUMZONESTOK 262
#define SUBTOK 263
#define TAGTOK 264
#define MESHTAGTYPE 265
#define MATERIALTOK 266
#define INDEXMODE 267
#define SUBTYPE 268
#define NUMBER 269
#define WORD 270
#define NAME 271
#define COMMENT 272




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 21 "cmgparse.y"
{
  Subdivision *subdivisionVal;
  char *strVal;
  Range rangeVal;
  IntList* intListPtr;
  int intVal;
  double doubleVal;
}
/* Line 1529 of yacc.c.  */
#line 92 "cmgparse.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

