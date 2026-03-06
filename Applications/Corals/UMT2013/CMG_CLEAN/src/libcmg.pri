include (../all.pri)



TEMPLATE = lib

CONFIG += staticlib

#Defines for the cmg library
DEFINES += \

SOURCES = \
lex.yy.c \
cmgparse.tab.c \
meshAndInputData.c \
CMGDomainQuery.c \
CMGIO.c \
CMGGenerator.c \
CMGMeshQuery.c \
CMGGlobalMeshQuery.c \
CMGTagQuery.c \
CMGMeshTopology.c \
dataTypes.c \
fortranUtilities.c \
subdivision.c \

# Our lexer file is here
#LEXSOURCES = cmgparse.lex

#And our parser
#YACCSOURCES = cmgparse.y

#From "Undocumented QMAKE"
#QMAKE_LEX = flex

#QMAKE_EXT_LEX = .c

#QMAKE_YACC = bison

#linux-g++ {
#QMAKE_YACCFLAGS = -d -o y.tab.c
#QMAKE_YACC_HEADER =
#QMAKE_YACC_SOURCE =
#}

#Add a target to generate the parser

#This target generates the following three source files
parser.target = cmgparse.tab.c cmgparse.tab.h lex.yy.c
#To generate them, run these commands
parser.commands = bison -d cmgparse.y ; flex cmgparse.lex
#This target depends on the source lex and yacc files
parser.depends = cmgparse.y cmgparse.lex

QMAKE_EXTRA_UNIX_TARGETS += parser

PRE_TARGETDEPS = cmgparse.tab.c cmgparse.tab.h lex.yy.c

