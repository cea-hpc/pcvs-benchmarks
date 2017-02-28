include ../../../Makevars

OBJS = matrix3D.o

EXECS = matrix3D

default: $(EXECS)

matrix3D: $(OBJS)
	$(FLINKER) $(LFLAGS) -o matrix3D $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

