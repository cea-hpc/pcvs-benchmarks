include ../../../Makevars

OBJS = singleI.o

EXECS = singleI

default: $(EXECS)

singleI: $(OBJS)
	$(FLINKER) $(LFLAGS) -o singleI $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

