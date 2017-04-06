topsrcdir=../../..

include ../../../make.defs

INCPATH += -I. -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities

CMI_SOURCES = MeshBase.cc ZoneBase.cc CornerBase.cc FaceBase.cc SideBase.cc 
CMI_OBJECTS = $(CMI_SOURCES:.cc=.o)

all: $(CMI_OBJECTS)
deps: MeshBase.d ZoneBase.d CornerBase.d FaceBase.d SideBase.d


-include MeshBase.d
-include ZoneBase.d
-include CornerBase.d
-include FaceBase.d
-include SideBase.d

clean:
	rm -f *.o *.pdb *.inst.*

veryclean: clean
	rm -f *.d
