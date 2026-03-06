include ../../make.defs

INCPATH += -I../src -I.

ifeq ($(CLASS),linux)
LIBPATH += -L./ -Xlinker -rpath -Xlinker . \
	-L../src -Xlinker -rpath -Xlinker ..//src \
	-L/usr/lib/mpi/lib -Xlinker -rpath -Xlinker /usr/lib/mpi/lib
endif

ifeq ($(CLASS),peloton)
LIBPATH += -L../src -Wl,-rpath ../src
endif

ifeq ($(CLASS),aix)
LIBPATH += -L./ -L../src
endif

LIBS += -lcmgp 

test:testApp.o 
	$(LINK) $(LINKFLAGS) -o $@ $< -L. $(LIBPATH) $(LIBS)

clean:
	rm -f *.o test

veryclean:clean
