topsrcdir=../..

include ../../make.defs

INCPATH += -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src

PART_SOURCES = OpacityBase.cc PartInst.cc
PART_OBJECTS = $(PART_SOURCES:.cc=.o)

all: $(PART_OBJECTS)

deps:  OpacityBase.d PartInst.d

-include PartInst.d
-include OpacityBase.d

clean:
	rm -f *.o *.pdb *.inst.*

veryclean: clean
	rm -f *.d
