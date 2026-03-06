/* A Bison parser, made by GNU Bison 1.875c.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



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




/* Copy the first part of user declarations.  */
#line 1 "cmgparse.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "parseStructs.h"
  extern SuperMeshSize sms;
  extern SubBlockContainer blocks;
  extern NumZones numZones;
  extern MeshTagContainer meshTags;
  extern SubdivisionContainer subdivisions;
  extern NodeDataContainer nodeData;
  extern ZoneDataContainer zoneData;
  extern unsigned int baseSeed;
  

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 21 "cmgparse.y"
typedef union YYSTYPE {
  Subdivision *subdivisionVal;
  char *strVal;
  Range rangeVal;
  IntList* intListPtr;
  int intVal;
  double doubleVal;
} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 136 "cmgparse.tab.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 148 "cmgparse.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

# ifndef YYFREE
#  define YYFREE free
# endif
# ifndef YYMALLOC
#  define YYMALLOC malloc
# endif

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   define YYSTACK_ALLOC alloca
#  endif
# else
#  if defined (alloca) || defined (_ALLOCA_H)
#   define YYSTACK_ALLOC alloca
#  else
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   155

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  23
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  14
/* YYNRULES -- Number of rules. */
#define YYNRULES  35
/* YYNRULES -- Number of states. */
#define YYNSTATES  147

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   272

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    21,     2,     2,
      18,    19,     2,     2,    20,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    22,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned char yyprhs[] =
{
       0,     0,     3,     4,     7,     9,    11,    13,    15,    17,
      19,    21,    23,    28,    37,    48,    57,    72,    74,    78,
      92,   102,   112,   122,   132,   142,   152,   162,   172,   182,
     192,   202,   217,   230,   234,   236
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      24,     0,    -1,    -1,    24,    25,    -1,    27,    -1,    28,
      -1,    29,    -1,    31,    -1,    33,    -1,    34,    -1,    26,
      -1,    36,    -1,     3,    18,    14,    19,    -1,     4,    18,
      14,    20,    14,    20,    14,    19,    -1,     5,    18,     6,
      20,    35,    20,    35,    20,    35,    19,    -1,     7,    18,
      14,    20,    14,    20,    14,    19,    -1,     7,    18,    18,
      30,    19,    20,    18,    30,    19,    20,    18,    30,    19,
      19,    -1,    14,    -1,    30,    20,    14,    -1,     8,    18,
      14,    21,    20,    35,    20,    35,    20,    35,    20,    32,
      19,    -1,    18,    14,    20,    14,    20,    32,    20,    14,
      19,    -1,    18,    14,    20,    32,    20,    32,    20,    14,
      19,    -1,    18,    32,    20,    32,    20,    14,    20,    14,
      19,    -1,    18,    32,    20,    14,    20,    14,    20,    14,
      19,    -1,    18,    14,    20,    14,    20,    32,    20,    32,
      19,    -1,    18,    14,    20,    32,    20,    32,    20,    32,
      19,    -1,    18,    32,    20,    14,    20,    32,    20,    14,
      19,    -1,    18,    14,    20,    14,    20,    32,    20,    32,
      19,    -1,    18,    14,    20,    32,    20,    14,    20,    32,
      19,    -1,    18,    14,    20,    32,    20,    14,    20,    14,
      19,    -1,    18,    14,    20,    14,    20,    14,    20,    14,
      19,    -1,     9,    18,    16,    20,    10,    20,    18,    35,
      20,    35,    20,    35,    19,    19,    -1,    11,    18,    16,
      20,    18,    35,    20,    35,    20,    35,    19,    19,    -1,
      14,    22,    14,    -1,    14,    -1,    17,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,    38,    38,    39,    43,    45,    47,    49,    51,    53,
      55,    57,    60,    78,   106,   134,   169,   213,   214,   217,
     275,   277,   279,   281,   283,   285,   287,   289,   291,   293,
     295,   299,   329,   352,   358,   366
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SEEDTOK", "SMSTOK", "BLKTOK", "STATE",
  "NUMZONESTOK", "SUBTOK", "TAGTOK", "MESHTAGTYPE", "MATERIALTOK",
  "INDEXMODE", "SUBTYPE", "NUMBER", "WORD", "NAME", "COMMENT", "'('",
  "')'", "','", "'%'", "':'", "$accept", "commands", "command", "seed_set",
  "sms_set", "blk_set", "numzones", "numberlist", "sub_set", "subdivision",
  "tag_create", "material_create", "range", "comment", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,    40,    41,
      44,    37,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    23,    24,    24,    25,    25,    25,    25,    25,    25,
      25,    25,    26,    27,    28,    29,    29,    30,    30,    31,
      32,    32,    32,    32,    32,    32,    32,    32,    32,    32,
      32,    33,    34,    35,    35,    36
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     8,    10,     8,    14,     1,     3,    13,
       9,     9,     9,     9,     9,     9,     9,     9,     9,     9,
       9,    14,    12,     3,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       2,     0,     1,     0,     0,     0,     0,     0,     0,     0,
      35,     3,    10,     4,     5,     6,     7,     8,     9,    11,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    12,     0,     0,     0,    17,
       0,     0,     0,     0,     0,    34,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    18,     0,
       0,     0,     0,    33,     0,     0,     0,     0,     0,     0,
      13,     0,    15,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    32,     0,     0,     0,    19,
       0,    16,     0,     0,    31,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    20,    24,
      29,    28,    21,    25,    23,    26,    22
};

/* YYDEFGOTO[NTERM-NUM]. */
static const yysigned_char yydefgoto[] =
{
      -1,     1,    11,    12,    13,    14,    15,    40,    16,    93,
      17,    18,    46,    19
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -78
static const short yypact[] =
{
     -78,     3,   -78,   -14,    -2,    17,    33,    44,    45,    46,
     -78,   -78,   -78,   -78,   -78,   -78,   -78,   -78,   -78,   -78,
      51,    52,    49,    -5,    53,    54,    55,    50,    48,    56,
      57,    58,    59,    61,    62,   -78,    60,    64,    65,   -78,
      37,    63,    74,    67,    66,    68,    69,    71,    72,    73,
      64,    75,    64,    79,    80,    64,    82,    70,   -78,    77,
      81,    78,    83,   -78,    84,    86,    58,    64,    64,    64,
     -78,    64,   -78,    39,    87,    88,    89,    91,    92,    64,
      64,    64,   -78,    85,    93,    94,    96,    58,    98,    64,
      99,    41,     9,   100,   101,   -78,   102,    97,   103,   -78,
     105,   -78,    10,    22,   -78,   106,   107,   108,   109,    23,
      24,    29,   111,   110,   112,   113,   114,   115,   116,   117,
     124,    34,    35,    36,   125,   126,   127,   123,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   -78,   -78,   -78,
     -78,   -78,   -78,   -78,   -78,   -78,   -78
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -78,   -78,   -78,   -78,   -78,   -78,   -78,   -65,   -78,   -77,
     -78,   -78,   -50,   -78
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned char yytable[] =
{
      59,    73,    61,     2,    20,    64,     3,     4,     5,    30,
       6,     7,     8,    31,     9,    98,    21,    74,    75,    76,
      10,    77,    91,    97,   105,   106,   108,    92,    92,    84,
      85,    86,   114,   116,   118,    22,   107,   113,   115,    94,
      92,    92,    92,   117,   129,   131,   133,    92,   128,   130,
     132,    23,    92,    92,    92,    29,    48,    49,    78,    49,
      96,    49,    24,    25,    26,    27,    28,    32,    36,    35,
      33,    34,    39,     0,    44,     0,    37,    38,    45,    47,
      41,    42,    43,    50,    51,    52,    53,    58,    66,    55,
      54,    56,    57,    62,    63,    60,    65,    67,    69,    68,
       0,     0,    70,    87,    71,    72,     0,    79,    80,    81,
      82,     0,    83,    88,    89,    90,    92,   102,    95,    99,
     100,   101,     0,   103,   104,   119,   109,   110,   111,   112,
     120,     0,   121,   122,   123,   124,   125,   126,   127,   134,
     135,   136,   137,     0,     0,     0,     0,   138,   139,   140,
     141,   142,   143,   144,   145,   146
};

static const yysigned_char yycheck[] =
{
      50,    66,    52,     0,    18,    55,     3,     4,     5,    14,
       7,     8,     9,    18,    11,    92,    18,    67,    68,    69,
      17,    71,    87,    14,    14,   102,   103,    18,    18,    79,
      80,    81,   109,   110,   111,    18,    14,    14,    14,    89,
      18,    18,    18,    14,   121,   122,   123,    18,    14,    14,
      14,    18,    18,    18,    18,     6,    19,    20,    19,    20,
      19,    20,    18,    18,    18,    14,    14,    14,    20,    19,
      16,    16,    14,    -1,    14,    -1,    20,    20,    14,    14,
      21,    20,    20,    20,    10,    18,    20,    14,    18,    20,
      22,    20,    20,    14,    14,    20,    14,    20,    20,    18,
      -1,    -1,    19,    18,    20,    19,    -1,    20,    20,    20,
      19,    -1,    20,    20,    20,    19,    18,    20,    19,    19,
      19,    19,    -1,    20,    19,    14,    20,    20,    20,    20,
      20,    -1,    20,    20,    20,    20,    20,    20,    14,    14,
      14,    14,    19,    -1,    -1,    -1,    -1,    19,    19,    19,
      19,    19,    19,    19,    19,    19
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,    24,     0,     3,     4,     5,     7,     8,     9,    11,
      17,    25,    26,    27,    28,    29,    31,    33,    34,    36,
      18,    18,    18,    18,    18,    18,    18,    14,    14,     6,
      14,    18,    14,    16,    16,    19,    20,    20,    20,    14,
      30,    21,    20,    20,    14,    14,    35,    14,    19,    20,
      20,    10,    18,    20,    22,    20,    20,    20,    14,    35,
      20,    35,    14,    14,    35,    14,    18,    20,    18,    20,
      19,    20,    19,    30,    35,    35,    35,    35,    19,    20,
      20,    20,    19,    20,    35,    35,    35,    18,    20,    20,
      19,    30,    18,    32,    35,    19,    19,    14,    32,    19,
      19,    19,    20,    20,    19,    14,    32,    14,    32,    20,
      20,    20,    20,    14,    32,    14,    32,    14,    32,    14,
      20,    20,    20,    20,    20,    20,    20,    14,    14,    32,
      14,    32,    14,    32,    14,    14,    14,    19,    19,    19,
      19,    19,    19,    19,    19,    19,    19
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)		\
   ((Current).first_line   = (Rhs)[1].first_line,	\
    (Current).first_column = (Rhs)[1].first_column,	\
    (Current).last_line    = (Rhs)[N].last_line,	\
    (Current).last_column  = (Rhs)[N].last_column)
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
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
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
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if defined (YYMAXDEPTH) && YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
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
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

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
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

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

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
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
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
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
        case 12:
#line 61 "cmgparse.y"
    {
  int seed = yyvsp[-1].intVal;
  if (seed < 0) {
    printf("Seed value must be non-negative");
    break;
  }
  unsigned int unsignedSeed = (unsigned int) seed;
  if ( unsignedSeed < UINT_MAX) {
    baseSeed = unsignedSeed;
  }
  else {
    printf("Seed value must be smaller than UINT_MAX = %d",UINT_MAX);
    break;
  }
;}
    break;

  case 13:
#line 79 "cmgparse.y"
    {
  if (sms.i != 0) {
    yyerror("sms has already been called");
    break;
  }

  int iSize = yyvsp[-5].intVal, jSize = yyvsp[-3].intVal, kSize = yyvsp[-1].intVal;

/*   printf("MAKING sms(%d,%d,%d)\n",iSize, jSize, kSize); */

  if ((iSize < 1)||(jSize < 1)||(kSize < 1)) {
    yyerror("The block size must be greater than zero.");
    break;
  }

  sms.i = iSize;
  sms.j = jSize;
  sms.k = kSize;
  numZones.iZones = (int *)malloc(sizeof(int)*sms.i);
  numZones.jZones = (int *)malloc(sizeof(int)*sms.j);
  numZones.kZones = (int *)malloc(sizeof(int)*sms.k);
  
  /*Nasty way to mark uninitialized array*/
  numZones.iZones[0] = -1;
;}
    break;

  case 14:
#line 107 "cmgparse.y"
    {
  if (!SMSExists()) { break; }

  bool on = yyvsp[-7].intVal;

/*   if (on) { */
/*     printf("Creating block\n"); */
/*   } else { */
/*     printf("Removing block\n"); */
/*   } */

  /* Assign ranges and perform a cursory check */
  Range iRange = yyvsp[-5].rangeVal, jRange = yyvsp[-3].rangeVal, kRange = yyvsp[-1].rangeVal;
  if (!(RangeCheck(&iRange)&&RangeCheck(&jRange)&&RangeCheck(&kRange))) {
    break;
  }  
  SubBlock block;
  block.iRange = iRange;
  block.jRange = jRange;
  block.kRange = kRange;
  block.on = on;

  SubBlockContainerAdd(&blocks, &block);

;}
    break;

  case 15:
#line 135 "cmgparse.y"
    {
  if (!SMSExists()) { break; }


  if (numZones.iZones[0] != -1) {
    fprintf(stderr, "The number of zones has already been set");
    break;
  }
  int iNumZones = yyvsp[-5].intVal, jNumZones = yyvsp[-3].intVal, kNumZones = yyvsp[-1].intVal;

  /* printf("Setting numzones %d %d %d",iNumZones, jNumZones, kNumZones); */

  if ((iNumZones < 0)||(jNumZones < 0)||(kNumZones < 0)) {
    fprintf(stderr, "The number of zones must be greater than or equal to zero.");
    break;
  }

  if (sms.i == 0) {
    fprintf(stderr, "Must call sms before setting numzones");
    break;
  }

  int ii;
  for (ii = 0; ii < sms.i; ++ii) {
    numZones.iZones[ii] = yyvsp[-5].intVal;
  }
  for (ii = 0; ii < sms.j; ++ii) {
    numZones.jZones[ii] = yyvsp[-3].intVal;
  }
  for (ii = 0; ii < sms.k; ++ii) {
    numZones.kZones[ii] = yyvsp[-1].intVal;
  }

;}
    break;

  case 16:
#line 170 "cmgparse.y"
    { 
  if (!SMSExists()) { break; }

  if (numZones.iZones[0] != -1) {
    fprintf(stderr, "The number of zones has already been set");
    break;
  }
 /*  printf("Setting numzones of ("); */
/*   printIntList($4);printf("), ("); */
/*   printIntList($8);printf("), ("); */
/*   printIntList($12);printf(")\n"); */
  if ((!copyIntListToArray( yyvsp[-10].intListPtr, sms.i, numZones.iZones))||
      (!copyIntListToArray( yyvsp[-6].intListPtr, sms.j, numZones.jZones))||
      (!copyIntListToArray(yyvsp[-2].intListPtr, sms.k, numZones.kZones))) {
    fprintf(stderr, "Must have the number of zones corresponding to the size of the superblock");
    break;
  }

  /*Check and make sure they are all positive*/
  int ii;
  for (ii = 0; ii < sms.i; ++ii) {
    /* printf("%d,",numZones.iZones[ii]); */
    if (numZones.iZones[ii] < 0) {
      fprintf(stderr, "The number of I zones (%d) must be greater than or equal to zero.", numZones.iZones[ii]);
      break;
    }
  }
  for (ii = 0; ii < sms.j; ++ii) {
    if (numZones.jZones[ii] < 0) {
      fprintf(stderr, "The number of J zones (%d) must be greater than or equal to zero.",numZones.jZones[ii]);
      break;
    }
  }
  for (ii = 0; ii < sms.k; ++ii) {
    if (numZones.kZones[ii] < 0) {
      fprintf(stderr, "The number of K zones (%d) must be greater than or equal to zero.",numZones.kZones[ii]);
      break;
    }
  }
;}
    break;

  case 17:
#line 213 "cmgparse.y"
    { yyval.intListPtr = initIntList(yyvsp[0].intVal, (IntList *)NULL); ;}
    break;

  case 18:
#line 214 "cmgparse.y"
    { yyval.intListPtr = initIntList( yyvsp[0].intVal, (IntList *)(yyvsp[-2].intListPtr)); ;}
    break;

  case 19:
#line 218 "cmgparse.y"
    {
  Subdivision *subdivision = yyvsp[-1].subdivisionVal;

  double percentage = (double)yyvsp[-10].intVal;
  if (percentage <= 0) {
    fprintf(stderr, "subdivision percentage must be greater than zero");
    break;
  }
  subdivision->fraction = percentage / 100;

  /* Assign ranges and perform a cursory check */
  Range iRange = yyvsp[-7].rangeVal, jRange = yyvsp[-5].rangeVal, kRange = yyvsp[-3].rangeVal;
  if (!(RangeCheck(&iRange)&&RangeCheck(&jRange)&&RangeCheck(&kRange))) {
    break;
  }  

  subdivision->iRange = iRange;
  subdivision->jRange = jRange;
  subdivision->kRange = kRange;

  SubdivisionContainerAdd(&subdivisions, subdivision);
;}
    break;

  case 20:
#line 276 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, yyvsp[-7].intVal, yyvsp[-5].intVal, -1, yyvsp[-1].intVal,  NULL, NULL, yyvsp[-3].subdivisionVal, NULL ); ;}
    break;

  case 21:
#line 278 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, yyvsp[-7].intVal, -1, -1, yyvsp[-1].intVal,  NULL, yyvsp[-5].subdivisionVal, yyvsp[-3].subdivisionVal, NULL ); ;}
    break;

  case 22:
#line 280 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, -1, -1, yyvsp[-3].intVal, yyvsp[-1].intVal,  yyvsp[-7].subdivisionVal, yyvsp[-5].subdivisionVal, NULL, NULL ); ;}
    break;

  case 23:
#line 282 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, -1, yyvsp[-5].intVal, yyvsp[-3].intVal, yyvsp[-1].intVal,  yyvsp[-7].subdivisionVal, NULL, NULL, NULL ); ;}
    break;

  case 24:
#line 284 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, yyvsp[-7].intVal, yyvsp[-5].intVal, -1, -1,  NULL, NULL, yyvsp[-3].subdivisionVal, yyvsp[-1].subdivisionVal ); ;}
    break;

  case 25:
#line 286 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, yyvsp[-7].intVal, -1, -1, -1,  NULL, yyvsp[-5].subdivisionVal, yyvsp[-3].subdivisionVal, yyvsp[-1].subdivisionVal ); ;}
    break;

  case 26:
#line 288 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, -1, yyvsp[-5].intVal, -1, yyvsp[-1].intVal,  yyvsp[-7].subdivisionVal, NULL, yyvsp[-3].subdivisionVal, NULL ); ;}
    break;

  case 27:
#line 290 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, yyvsp[-7].intVal, yyvsp[-5].intVal, -1, -1,  NULL, NULL, yyvsp[-3].subdivisionVal, yyvsp[-1].subdivisionVal ); ;}
    break;

  case 28:
#line 292 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, yyvsp[-7].intVal, -1, yyvsp[-3].intVal, -1,  NULL, yyvsp[-5].subdivisionVal, NULL, yyvsp[-1].subdivisionVal ); ;}
    break;

  case 29:
#line 294 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, yyvsp[-7].intVal, -1, yyvsp[-3].intVal, yyvsp[-1].intVal,  NULL, yyvsp[-5].subdivisionVal, NULL, NULL ); ;}
    break;

  case 30:
#line 296 "cmgparse.y"
    { yyval.subdivisionVal = SubdivisionInit(-1.0, yyvsp[-7].intVal, yyvsp[-5].intVal, yyvsp[-3].intVal, yyvsp[-1].intVal,  NULL, NULL, NULL, NULL ); ;}
    break;

  case 31:
#line 299 "cmgparse.y"
    {
  /* printf("Found a tag\n"); */
  if (!SMSExists()) { break; }

  MeshTag tag;

  const char *typeString = (const char *)yyvsp[-9].strVal;

  if (     !strcmp(typeString, "node")) { tag.meshTagType = CMG_NODE; }
  else if (!strcmp(typeString, "edge")) { tag.meshTagType = CMG_EDGE; }
  else if (!strcmp(typeString, "face")) { tag.meshTagType = CMG_FACE; }
  else if (!strcmp(typeString, "zone")) { tag.meshTagType = CMG_ZONE; }
  else {
    fprintf(stderr, "Bad tag type");
  }

  /*Copy name into tag*/
  strcpy(tag.name, (const char *)yyvsp[-11].strVal);

  Range iRange = yyvsp[-6].rangeVal, jRange = yyvsp[-4].rangeVal, kRange = yyvsp[-2].rangeVal;
  if (!(RangeCheck(&iRange) && RangeCheck(&jRange) && RangeCheck(&kRange))) {
    break;
  }
  tag.iRange = iRange; tag.jRange = jRange; tag.kRange = kRange;
  tag.faceBaseIndex = -1;

  MeshTagContainerAdd(&meshTags, &tag);
;}
    break;

  case 32:
#line 330 "cmgparse.y"
    {
  if (!SMSExists()) { break; }

  MeshTag tag;
  
  tag.meshTagType = CMG_MATERIAL;

  /*Copy name into tag*/
  strcpy(tag.name, (const char *)yyvsp[-9].strVal);
  
  Range iRange = yyvsp[-6].rangeVal, jRange = yyvsp[-4].rangeVal, kRange = yyvsp[-2].rangeVal;
  if (!(RangeCheck(&iRange) && RangeCheck(&jRange) && RangeCheck(&kRange))) {
    break;
  }
  tag.iRange = iRange; tag.jRange = jRange; tag.kRange = kRange;
  tag.faceBaseIndex = -1;

  MeshTagContainerAdd(&meshTags, &tag);

;}
    break;

  case 33:
#line 353 "cmgparse.y"
    {
  yyval.rangeVal.min = yyvsp[-2].intVal;
  yyval.rangeVal.max = yyvsp[0].intVal;
  /*  RangePrint( $$ ); */
;}
    break;

  case 34:
#line 359 "cmgparse.y"
    {
  /*If only one number is specified, the min and max are both that number */
  yyval.rangeVal.min = yyvsp[0].intVal;
  yyval.rangeVal.max = yyvsp[0].intVal;
;}
    break;

  case 35:
#line 367 "cmgparse.y"
    {
  /* printf("Ignoring comment '%s'\n", $1); */
;}
    break;


    }

/* Line 1000 of yacc.c.  */
#line 1464 "cmgparse.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


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
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  const char* yyprefix;
	  char *yymsg;
	  int yyx;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 0;

	  yyprefix = ", expecting ";
	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		yysize += yystrlen (yyprefix) + yystrlen (yytname [yyx]);
		yycount += 1;
		if (yycount == 5)
		  {
		    yysize = 0;
		    break;
		  }
	      }
	  yysize += (sizeof ("syntax error, unexpected ")
		     + yystrlen (yytname[yytype]));
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yyprefix = ", expecting ";
		  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			yyp = yystpcpy (yyp, yyprefix);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yyprefix = " or ";
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* If at end of input, pop the error token,
	     then the rest of the stack, then return failure.  */
	  if (yychar == YYEOF)
	     for (;;)
	       {
		 YYPOPSTACK;
		 if (yyssp == yyss)
		   YYABORT;
		 YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
		 yydestruct (yystos[*yyssp], yyvsp);
	       }
        }
      else
	{
	  YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
	  yydestruct (yytoken, &yylval);
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

#ifdef __GNUC__
  /* Pacify GCC when the user code never invokes YYERROR and the label
     yyerrorlab therefore never appears in user code.  */
  if (0)
     goto yyerrorlab;
#endif

  yyvsp -= yylen;
  yyssp -= yylen;
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
      if (yyn != YYPACT_NINF)
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

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


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

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 372 "cmgparse.y"


int yywrap()
{
  return 1;
}

IntList *initIntList( int val, IntList *next) {
  IntList *node;
  node = (IntList *)malloc(sizeof(IntList));

  node->val = val;
  node->next = next;

  return node;
}

void printIntList( IntList  *node ) {
  printf("%d, ",node->val);
  if (node->next != NULL) {
    printIntList( node->next ) ;
  } else {
    return;
  }
}

/* Copy the contents of the list */
bool copyIntListToArray( IntList *node, int size, int *array) {
  /* Our lists come in backwards */
  int ii;
  for (ii = 0; ii < size; ++ii) {
    if (node == NULL) {
      /*fprintf(stderr, "Invalid pointer in copyIntListToArray\n");*/
      return false;
    }
    array[size-ii-1] = node->val;
    node = node->next;
    
  }
  return true;
}

/* Return false if the sms has not been created yet */
bool SMSExists(void) {
  if (sms.i == 0) {
   fprintf(stderr, "Must create the superblock with the sms command first");
   return false;
  }
  else {
    return true;
  }
}

