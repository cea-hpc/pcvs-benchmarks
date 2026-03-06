topsrcdir=../..

include ../../make.defs

INCPATH += -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src

COMM_SOURCES = DomainNeighborMapInst.cc CommAgent.cc
COMM_OBJECTS = $(COMM_SOURCES:.cc=.o)

all: $(COMM_OBJECTS)

deps: DomainNeighborMapInst.d CommAgent.d

-include DomainNeighborMapInst.d
-include CommAgent.d

clean:
	rm -f *.o libCommunication.$(LIB_EXT)

veryclean: clean
	rm -f *.d *.pdb *.inst.*
