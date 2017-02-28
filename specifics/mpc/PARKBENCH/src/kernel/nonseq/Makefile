include ../../../Makevars

OBJS = nonseq.o

EXECS = nonseq

default: $(EXECS)

nonseq: $(OBJS)
	$(FLINKER) $(LFLAGS) -o nonseq $(OBJS) $(PROFLIBS) $(LIBS)

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS)
#	-/bin/rm -f *~ core $(OBJS)

veryclean:
	-make clean
	-/bin/rm -f $(EXECS)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

