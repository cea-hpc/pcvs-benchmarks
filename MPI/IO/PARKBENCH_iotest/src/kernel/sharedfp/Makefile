include ../../../Makevars

OBJS = sharedfp.o

EXECS = sharedfp

default: $(EXECS)

sharedfp: $(OBJS)
	$(FLINKER) $(LFLAGS) -o sharedfp $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

