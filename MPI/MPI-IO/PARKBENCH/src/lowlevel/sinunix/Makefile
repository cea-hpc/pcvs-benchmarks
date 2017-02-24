include ../../../Makevars

OBJS = sinunix.o

EXECS = sinunix

default: $(EXECS)

sinunix: $(OBJS)
	$(FLINKER) $(LFLAGS) -o sinunix $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

