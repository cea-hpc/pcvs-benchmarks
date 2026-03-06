topsrcdir=../..

include ../../make.defs

INCPATH += -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src

UTILITIES_SOURCES = DBC.cc VERIFY.cc Process.cc
UTILITIES_OBJECTS = $(UTILITIES_SOURCES:.cc=.o)

all: $(UTILITIES_OBJECTS)

deps:  $(UTILITIES_SOURCES:.cc=.d)

-include DBC.d
-include VERIFY.d
-include Process.d

clean:
	rm -f *.o *.pdb *.inst.*

veryclean: clean
	rm -f *.d
