SHELL=/bin/sh
BENCHMARK=lu-mz
BENCHMARKU=LU-MZ
VEC=

include ../config/make.def

OBJS = lu.o read_input.o mpi_setup.o \
       setcoeff.o setbv.o exact.o setiv.o \
       erhs.o ssor$(VEC).o zone_setup.o exch_qbc.o \
       rhs$(VEC).o l2norm.o jacld.o blts$(VEC).o jacu.o buts$(VEC).o \
       syncs.o error.o pintgr.o verify.o ${COMMON}/print_results.o \
       ${COMMON}/timers.o

include ../sys/make.common


# npbparams.h is included by header.h
# The following rule should do the trick but many make programs (not gmake)
# will do the wrong thing and rebuild the world every time (because the
# mod time on header.h is not changed. One solution would be to 
# touch header.h but this might cause confusion if someone has
# accidentally deleted it. Instead, make the dependency on npbparams.h
# explicit in all the lines below (even though dependence is indirect). 

# header.h: npbparams.h

${PROGRAM}: config
	@if [ x$(VERSION) = xvec ] ; then	\
		${MAKE} VEC=_vec exec;		\
	elif [ x$(VERSION) = xVEC ] ; then	\
		${MAKE} VEC=_vec exec;		\
	else					\
		${MAKE} exec;			\
	fi

exec: $(OBJS)
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB}

.f.o :
	${FCOMPILE} $<

lu.o:		lu.f mpi_stuff.h header.h npbparams.h
blts$(VEC).o:	blts$(VEC).f
buts$(VEC).o:	buts$(VEC).f
erhs.o:		erhs.f header.h npbparams.h
zone_setup.o:   zone_setup.f mpi_stuff.h header.h npbparams.h
exch_qbc.o:     exch_qbc.f mpi_stuff.h header.h npbparams.h
error.o:	error.f
exact.o:	exact.f header.h npbparams.h
jacld.o:	jacld.f header.h npbparams.h
jacu.o:		jacu.f header.h npbparams.h
l2norm.o:	l2norm.f
pintgr.o:	pintgr.f header.h npbparams.h
read_input.o:	read_input.f mpi_stuff.h header.h npbparams.h
rhs$(VEC).o:	rhs$(VEC).f header.h npbparams.h
setbv.o:	setbv.f header.h npbparams.h
setiv.o:	setiv.f header.h npbparams.h
setcoeff.o:	setcoeff.f header.h npbparams.h
ssor$(VEC).o:	ssor$(VEC).f header.h npbparams.h
verify.o:	verify.f header.h npbparams.h
mpi_setup.o:	mpi_setup.f mpi_stuff.h header.h npbparams.h
syncs.o:	syncs.f

clean:
	- /bin/rm -f npbparams.h
	- /bin/rm -f *.o *~
