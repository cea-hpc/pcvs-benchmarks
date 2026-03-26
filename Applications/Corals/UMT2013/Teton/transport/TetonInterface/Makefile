topsrcdir=../../..

include ../../../make.defs

INCPATH += -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities

TETON_INTERFACE_OBJECTS = TetonInst.o TetonNT.o

all: $(TETON_INTERFACE_OBJECTS)
deps: TetonInst.d

clean:
	rm -f *.o *.pdb *.inst.*

veryclean: clean
	rm -f *.d

-include TetonInst.d
