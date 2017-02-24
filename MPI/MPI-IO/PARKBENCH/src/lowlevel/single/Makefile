include ../../../Makevars

OBJS = single.o

EXECS = single

default: $(EXECS)

single: $(OBJS)
	$(FLINKER) $(LFLAGS) -o single $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

