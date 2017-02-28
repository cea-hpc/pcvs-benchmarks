include ../../../Makevars

OBJS = gatherscat2D.o

EXECS = gatherscat2D

default: $(EXECS)

gatherscat2D: $(OBJS)
	$(FLINKER) $(LFLAGS) -o gatherscat2D $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

