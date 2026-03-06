topsrcdir = ../..

include $(topsrcdir)/make.defs

INCPATH += -I.

all: libcmgp.$(LIB_EXT)

LIB_SOURCES := meshAndInputData.c CMGDomainQuery.c \
CMGIO.c CMGGenerator.c CMGMeshQuery.c CMGGlobalMeshQuery.c CMGTagQuery.c \
CMGMeshTopology.c dataTypes.c fortranUtilities.c subdivision.c 

LIB_SOURCES += lex.yy.c cmgparse.tab.c
LIB_OBJECTS :=$(LIB_SOURCES:.c=.o)


#lex.yy.c: cmgparse.lex cmgparse.tab.c
#	flex cmgparse.lex

#cmgparse.tab.c: cmgparse.y
#	bison -d cmgparse.y

libcmgp.$(LIB_EXT): $(LIB_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^


clean:
	rm -f *.o libcmgp.* *.pdb *.inst.*

veryclean: clean   # don't remove generated files for now
# rm -f cmgparse.tab.c cmgparse.tab.h lex.yy.c
