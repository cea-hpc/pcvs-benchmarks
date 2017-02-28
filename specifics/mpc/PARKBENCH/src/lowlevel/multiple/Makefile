include ../../../Makevars

OBJS = multiple.o

EXECS = multiple

default: $(EXECS)

multiple: $(OBJS)
	$(FLINKER) $(LFLAGS) -o multiple $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

