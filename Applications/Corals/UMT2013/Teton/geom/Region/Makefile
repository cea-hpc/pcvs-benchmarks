topsrcdir=../../..

include ../../../make.defs

INCPATH += -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src

REGION_SOURCES = RegionInst.cc
REGION_OBJECTS = $(REGION_SOURCES:.cc=.o)

all: $(REGION_OBJECTS)

deps: RegionInst.d

-include RegionInst.d

clean:
	rm -f *.o *.pdb *.inst.*

veryclean: clean
	rm -f *.d
