include ../../../Makevars

OBJS = transpose.o

EXECS = transpose

default: $(EXECS)

transpose: $(OBJS)
	$(FLINKER) $(LFLAGS) -o transpose $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

