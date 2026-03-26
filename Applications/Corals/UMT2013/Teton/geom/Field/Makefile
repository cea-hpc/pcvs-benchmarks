topsrcdir=../../..

include ../../../make.defs

INCPATH += -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src

all: FieldInst.o
deps: FieldInst.d

clean:
	rm -f *.o *.pdb *.inst.*

veryclean: clean
	rm -f *.d

-include FieldInst.d
