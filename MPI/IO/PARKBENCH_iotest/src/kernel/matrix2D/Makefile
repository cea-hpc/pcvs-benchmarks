include ../../../Makevars

OBJS = matrix2D.o

EXECS = matrix2D

default: $(EXECS)

matrix2D: $(OBJS)
	$(FLINKER) $(LFLAGS) -o matrix2D $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

